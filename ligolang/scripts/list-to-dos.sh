#!/bin/sh

cd "$(git rev-parse --show-cdup)"

escapehtml() {
  sed -e 's/&/\&amp;/g' \
      | sed -e 's/</\&lt;/g' \
      | sed -e 's/>/\&gt;/g' \
      | sed -e 's/"/\&quot;/g'
}

cat <<'EOF'
<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml" lang="en" xml:lang="en">
  <head>
    <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
    <title>LIGO to-dos</title>
    <style>
    li code { font-size: larger; }
    li > a { color: black; }
    li pre { background-color: #eee; padding: 1em; border: thin solid gray; }
    </style>
  </head>
  <body>
EOF
printf %s '    <h1>'
git grep -i \\'(to''do'\\'|fix''me'\\')' | wc -l
printf %s ' LIGO to-dos</h1>'
printf %s '    <ul>'
  
commit="$(git rev-parse HEAD | escapehtml)"

to_hex() {
  xxd -ps | tr -d \\n\\r | tr 'ABCDEF' 'abcdef' | sed -e 's/../x&/g'
}

delimited_to_hex() {
  # $1 is the delimitor's hex code, e.g. x00 for null-delimited, stdin is the stream
  to_hex | sed -e s/"$1"/\\n/g
}

non_null_hex_to_str_end_x() {
  printf '%s' "$1" | sed -e 's/x//' | xxd -ps -r
  printf 'x'
}

ignorecase() {
  printf '%s' "$1" | to_hex | sed -e 's/x[46]/x[46]/g' | sed -e 's/x[57]/x[57]/g'
}

rea='s%^/(/(x3[0-9]/)*/)/(x3a.*/)/('"$(ignorecase 'to''do')"'/|'"$(ignorecase 'fix''me')"'/)/(.*/)$'
rea="$rea"'%'"$(printf '<a href="https://gitlab.com/ligolang/ligo/-/blob/dev/' | to_hex)"
reb="$(printf '#L' | to_hex)"'/1'"$(printf '">' | to_hex)"'/1/3'"$(printf '<strong>' | to_hex)"'/4'"$(printf '</strong>' | to_hex)"'/5'"$(printf '</a>' | to_hex)"
reb="$reb"'%g'
rea="$(printf '%s' "$rea" | tr '/' \\\\)"
reb="$(printf '%s' "$reb" | tr '/' \\\\)"

# No quotes becuse we iterate on newline-delimied hex
for filehex in $(git ls-files -z | delimited_to_hex x00); do
  file="$(non_null_hex_to_str_end_x "$filehex")"
  file="${file%x}"
  # No quotes becuse we iterate on newline-delimied hex
  title_printed=false
  for matcheshex in $(grep -C 5 -i -n \\'(to''do'\\'|fix''me'\\')' -- "$file" | tr \\0 ' ' | to_hex | sed -e s/x0ax2dx2dx0a/\\n/g); do
    if test "$title_printed" = "false"; then
      printf '%s' '<li><a href="https://gitlab.com/ligolang/ligo/-/blob/dev/'
      printf '%s' "$file" | escapehtml
      printf '%s' '"><code>'
      printf '%s' "$file" | escapehtml
      printf '%s'\\n '</code></a>'
      title_printed=true
    fi
    found="$(printf %s "$matcheshex" | grep -P -o 'x28x2a.*?x28x29')"
    if test "x$comments" = 'x'; then
      found="$matcheshex"
    fi
    printf '%s'\\n '<pre>'
    printf "$found" | sed -e s/x0a/\\n/g | sed -e "$rea$(printf %s "$file" | escapehtml | to_hex)$reb" | sed -e 's/x//' | tr \\n 'n' | sed -e 's/n/x0a/g' | xxd -ps -r
    printf '%s'\\n '</pre>'
  done
  printf '%s'\\n '</li>'
done
cat <<'EOF'
    </ul>
  </body>
</html>
EOF