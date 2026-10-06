#!/usr/bin/env python3
"""Copy Helix's grammar list and highlight queries into him's runtime/.

Usage: dev/sync-helix-runtime.py HELIX_CHECKOUT

A maintainer tool (ADR grammar-setup): users never need Helix. It writes
  runtime/grammars.toml          every [[grammar]] of Helix's languages.toml with a
                                 git source: one table per grammar (git, rev,
                                 subpath), the revisions Helix's queries are written for
  runtime/queries/*/highlights.scm
  runtime/queries/LICENSE        Helix's licence (MPL-2.0), which covers both
and records the Helix commit they came from in runtime/HELIX-VERSION.
Both are MPL-2.0 files inside a BSD-3-Clause project: they keep that licence,
and changes to them are MPL-2.0 too.
"""
import os
import shutil
import subprocess
import sys
import tomllib

KEY_SAFE = set('abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_-')


def quote(s):
    return '"' + s.replace('\\', '\\\\').replace('"', '\\"') + '"'


def key(s):
    return s if s and set(s) <= KEY_SAFE else quote(s)


def main(helix):
    him = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
    runtime = os.path.join(him, 'runtime')
    rev = subprocess.run(['git', '-C', helix, 'rev-parse', 'HEAD'],
                         capture_output=True, text=True, check=True).stdout.strip()
    with open(os.path.join(helix, 'languages.toml'), 'rb') as f:
        langs = tomllib.load(f)

    grammars = sorted((g for g in langs['grammar'] if 'git' in g['source']), key=lambda g: g['name'])
    out = [
        '# Tree-sitter grammars him can fetch and build (him --grammar fetch / build).',
        f'# From Helix\'s languages.toml at {rev} (MPL-2.0, see queries/LICENSE);',
        '# regenerate with dev/sync-helix-runtime.py, do not edit by hand.',
        '',
    ]
    for g in grammars:
        s = g['source']
        out.append(f'[{key(g["name"])}]')
        out.append(f'git = {quote(s["git"])}')
        out.append(f'rev = {quote(s["rev"])}')
        if 'subpath' in s:
            out.append(f'subpath = {quote(s["subpath"])}')
        out.append('')
    os.makedirs(runtime, exist_ok=True)
    with open(os.path.join(runtime, 'grammars.toml'), 'w') as f:
        f.write('\n'.join(out))

    queries = os.path.join(runtime, 'queries')
    shutil.rmtree(queries, ignore_errors=True)
    src = os.path.join(helix, 'runtime', 'queries')
    copied = 0
    for lang in sorted(os.listdir(src)):
        hl = os.path.join(src, lang, 'highlights.scm')
        if os.path.isfile(hl):
            os.makedirs(os.path.join(queries, lang))
            shutil.copyfile(hl, os.path.join(queries, lang, 'highlights.scm'))
            copied += 1
    shutil.copyfile(os.path.join(helix, 'LICENSE'), os.path.join(queries, 'LICENSE'))
    with open(os.path.join(runtime, 'HELIX-VERSION'), 'w') as f:
        f.write(rev + '\n')
    # A new query directory is not a dependency of Him.Embedded yet: rebuild it.
    os.utime(os.path.join(him, 'src', 'Him', 'Embedded.hs'))
    print(f'{len(grammars)} grammars, {copied} queries, from Helix {rev}')


if __name__ == '__main__':
    if len(sys.argv) != 2:
        sys.exit(__doc__)
    main(sys.argv[1])
