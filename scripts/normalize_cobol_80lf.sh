#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="${1:-.}"

normalize_file() {
  local file="$1"
  local tmp
  tmp="$(mktemp)"

  # Remove CR, truncate >80 and pad <80 with spaces. Always write LF line endings.
  awk '{
    line=$0
    sub(/\r$/, "", line)
    if (length(line) > 80) {
      line = substr(line, 1, 80)
    }
    printf "%-80s\n", line
  }' "$file" > "$tmp"

  mv "$tmp" "$file"
}

while IFS= read -r -d '' file; do
  normalize_file "$file"
done < <(find "$ROOT_DIR" -type f \( -name "*.COB" -o -name "*.cob" -o -name "*.CBL" -o -name "*.cbl" -o -name "*.COPY" -o -name "*.copy" -o -name "*.CPY" -o -name "*.cpy" -o -name "*.BMS" -o -name "*.bms" \) -print0)

echo "Normalization completed for COBOL/BMS files under: $ROOT_DIR"
