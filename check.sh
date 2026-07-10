#!/usr/bin/env bash
# 全*.hsファイルをコンパイルして実行するCI用チェックスクリプト。
# 成果物(バイナリ, *.hi, *.o)は一時ディレクトリに出力するためリポジトリには残らない。
# 使い方: nix develop --command ./check.sh
set -u

cd "$(dirname "$0")"

workdir=$(mktemp -d)
trap 'rm -rf "$workdir"' EXIT

failed=()

while IFS= read -r src; do
  name=$(basename "$src" .hs)
  outdir="$workdir/${src%.hs}"
  mkdir -p "$outdir"

  echo "=== $src"
  if ! ghc -Wall -outputdir "$outdir" -o "$outdir/$name" "$src"; then
    failed+=("$src (compile)")
    continue
  fi
  if ! "$outdir/$name" > /dev/null; then
    failed+=("$src (runtime)")
  fi
done < <(find . -name '*.hs' -not -path './.direnv/*' | sort)

echo
if ((${#failed[@]} > 0)); then
  echo "NG: ${#failed[@]} file(s) failed:"
  printf '  %s\n' "${failed[@]}"
  exit 1
fi
echo "OK: all files compiled and ran successfully."
