#!/usr/bin/env bash
set -euo pipefail

root_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
example="${1:-test1}"
output_dir="${2:-"$root_dir/data/outputs/$example"}"
script="$root_dir/examples/$example.tide"
brs_file="$output_dir/model.big"
states_dir="$output_dir/states"
transition_file="$output_dir/transitions.txt"
bigrapher="${BIGRAPHER:-$root_dir/vendor/bigraph-tools/_build/install/default/bin/bigrapher}"

if [[ ! -f "$script" ]]; then
  printf 'Example not found: %s\n' "$script" >&2
  exit 1
fi
if [[ ! -x "$bigrapher" ]]; then
  printf 'BigraphER executable not found: %s\n' "$bigrapher" >&2
  printf 'Set BIGRAPHER or build vendor/bigraph-tools first.\n' >&2
  exit 1
fi
command -v pdflatex >/dev/null ||
  { printf 'pdflatex is required to generate PDFs.\n' >&2; exit 1; }

mkdir -p "$states_dir"
rm -f "$brs_file" "$transition_file" "$states_dir"/*.tikz "$states_dir"/*.pdf \
  "$states_dir"/*.aux "$states_dir"/*.log

(
  cd "$root_dir"
  cat "$script" | dune exec bin/main.exe -- --brs "$brs_file"
)

"$bigrapher" sim \
  --quiet \
  --format=tikz \
  --simulation-steps=5 \
  --export-states="$states_dir" \
  --export-ts="$transition_file" \
  "$brs_file"

shopt -s nullglob
tikz_files=("$states_dir"/*.tikz)
if ((${#tikz_files[@]} == 0)); then
  printf 'BigraphER did not generate any TikZ states.\n' >&2
  exit 1
fi

for tikz_file in "${tikz_files[@]}"; do
  base="${tikz_file##*/}"
  base="${base%.tikz}"
  pdflatex \
    -interaction=nonstopmode \
    -halt-on-error \
    -jobname="$base" \
    -output-directory="$states_dir" \
    "$tikz_file" >/dev/null
done

printf 'BRS: %s\nTikZ states: %s\nPDF states: %s\n' \
  "$brs_file" "$states_dir" "$states_dir"
