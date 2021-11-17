#!/bin/sh

# This script extracts the error states of an LR automaton produced by
# Menhir and generates minimal inputs that cover all of them and only
# them.

# set -x

# ====================================================================
# General Settings and wrappers

script=$(basename $0)

print_nl () { test "$quiet" != "yes" && echo "$1"; }

print () { test "$quiet" != "yes" && printf "$1";  }

fatal_error () {
  echo "$script: fatal error:"
  echo "$1" 1>&2
  exit 1
}

warn () {
 print_nl "$script: warning:"
 print_nl "$1"
}

failed () {
  printf "\033[31mFAILED$1\033[0m\n"
}

emphasise () {
  printf "\033[31m$1\033[0m\n"
}

# ====================================================================
# Parsing loop
#
while : ; do
  case "$1" in
    "") break;;
    --par-tokens=*)
      if test -n "$par_tokens"; then
        fatal_error "Repeated option --par-tokens."; fi
      par_tokens=$(expr "$1" : "[^=]*=\(.*\)")
      ;;
    --par-tokens)
      no_eq=$1
      break
      ;;
    --lex-tokens=*)
      if test -n "$lex_tokens"; then
        fatal_error "Repeated option --lex-tokens."; fi
      lex_tokens=$(expr "$1" : "[^=]*=\(.*\)")
      ;;
    --lex-tokens)
      no_eq=$1
      break
      ;;
    --ext=*)
      if test -n "$ext_opt"; then
        fatal_error "Repeated option --ext."; fi
      ext=$(expr "$1" : "[^=]*=\(.*\)")
      ;;
    --ext)
      no_eq=$1
      break
      ;;
    --dir=*)
      if test -n "$dir_opt"; then
        fatal_error "Repeated option --dir."; fi
      dir=$(expr "$1" : "[^=]*=\(.*\)")
      ;;
    --dir)
      no_eq=$1
      break
      ;;
      # Help
      #
    --concatenate*)
      if test -n "$dir_opt"; then
        fatal_error "Repeated option --concatenate."; fi
      concatenate=yes
      ;;
    --unlexer=*)
      if test -n "$unlexer"; then
        fatal_error "Repeated option --unlexer."; fi
      unlexer=$(expr "$1" : "[^=]*=\(.*\)")
      ;;
    --unlexer)
      no_eq=$1
      break
      ;;
    --messages=*)
      if test -n "$messages"; then
        fatal_error "Repeated option --messages."; fi
      messages=$(expr "$1" : "[^=]*=\(.*\)")
      ;;
    --messages)
      no_eq=$1
      break
      ;;
    -h | --help | -help)
      help=yes
      ;;
      # Invalid option
      #
    -*)
      fatal_error "Invalid option \"$1\"."
      ;;
      # Invalid argument
      #
     *)
      if test -n "$parser_arg"; then
        fatal_error "Only one Menhir specification allowed."; fi
      parser=$1
  esac
  shift
done

# ====================================================================
# Help
#
usage () {
  cat <<EOF
Usage: $(basename $0) [-h|--help] [--concatenate]
                --par-tokens=<par_tokens>.mly
                --lex-tokens=<lex_tokens>.mli
                --messages=<parser>.msg
                --unlexer=<binary>
                --ext=<extension>
                --dir=<path>
                <parser>.mly

Generates in directory <path> a set of LIGO source files with
extension <extension> covering all erroneous states of the LR
automaton produced by Menhir from <parser>.mly, <par_tokens>.mly,
<lex_tokens>.mli and <parser>.msg (see script messages.sh for
generating the latter). The LIGO files will be numbered with their
corresponding state number in the automaton. The executable <binary>
reads a line on stdin of tokens and produces a line of corresponding
lexemes.
If option --concatenate is used a single file 'all.<extension>' will
be produced, containing the concatenation of all the erroneous expressions

The following options, if given, must be given only once.

Display control:
  -h, --help  display this help and exit

Mandatory options:
      --lex-tokens=<name>.mli the lexical tokens
      --par-tokens=<name>.mly the syntactical tokens
      --messages=<parser>.msg the complete errors messages
      --ext=EXT               Unix file extension for the
                              generated LIGO files
                              (no starting period)
      --dir=PATH              directory to store the generated
                              LIGO files (no trailing slash)
      --unlexer=<binary>      from tokens to lexemes (one line on stdin)
EOF
  exit 1
}

if test "$help" = "yes"; then usage; fi

# ====================================================================
# Checking the command-line options and arguments and applying some of
# them.

# It is a common mistake to forget the "=" in GNU long-option style.

if test -n "$no_eq"
then
  fatal_error "Long option style $no_eq must be followed by \"=\"."
fi

# Checking options

if test -z "$messages"; then
  fatal_error "Messages not found (use --messages)."; fi

if test -z "$unlexer"; then
  fatal_error "Unlexer binary not found (use --unlexer)."; fi

if test -z "$parser"; then
  fatal_error "No parser specification."; fi

if test -z "$par_tokens"; then
  fatal_error "No syntactical tokens specification (use --par-tokens)."; fi

if test -z "$lex_tokens"; then
  fatal_error "No lexical tokens specification (use --lex-tokens)."; fi

if test ! -e "$messages"; then
  fatal_error "Error messages \"$messages\" not found (use messages.sh)."; fi

if test ! -e "$parser"; then
  fatal_error "Parser specification \"$parser\" not found."; fi

#if test ! -e "$lex_tokens"; then
#  fatal_error "Lexical tokens specification \"$lex_tokens\" not found."; fi

if test ! -e "$par_tokens"; then
  fatal_error "Syntactical tokens specification \"$par_tokens\" not found."; fi

parser_ext=$(expr "$parser" : ".*\.mly$")
if test "$parser_ext" = "0"; then
  fatal_error "Parser specification must have extension \".mly\"."; fi

par_tokens_ext=$(expr "$par_tokens" : ".*\.mly$")
if test "$par_tokens_ext" = "0"; then
  fatal_error "Syntactical tokens specification must have extension \".mly\"."
fi

lex_tokens_ext=$(expr "$lex_tokens" : ".*\.mli$")
if test "$lex_tokens_ext" = "0"; then
  fatal_error "Lexical tokens specification must have extension \".mli\"."
fi

mly=$parser
parser_base=$(basename $mly .mly)
par_tokens_base=$(basename $par_tokens .mly)
lex_tokens_base=$(basename $lex_tokens .mli)

# Checking the output directory

if test -z "$dir"; then
  fatal_error "No output directory (use --dir)."; fi

if test ! -d "$dir"; then
  fatal_error "Output directory \"$dir\" not found."; fi

# Checking the LIGO extension

if test -z "$ext"; then
  fatal_error "No LIGO extension (use --ext)."; fi

ext_start=$(expr "$ext" : "^\..*")
if test "$ext_start" != "0"
then fatal_error "LIGO extensions must not start with a period."
fi

# ====================================================================
# Menhir's flags

flags="--table --strict --external-tokens $lex_tokens_base \
       --base $parser_base $par_tokens"

# ====================================================================
# Producing erroneous sentences from Menhir's error messages

msg=$messages
raw=$parser_base.msg.raw
printf "Making $raw from $msg... "
menhir --echo-errors $parser_base.msg $flags $mly > $raw 2>/dev/null
sed -i -e 's/^.*: \(.*\)$/\1/g' $raw
printf "done.\n"

# ====================================================================
# Converting Menhir's minimal erroneous sentences to concrete syntax

printf "Unlexing the erroneous sentences... "
states=$msg.states
map=$msg.map
sed -n "s/.* state\: \([0-9]\+\)./\1/p" $msg > $states
paste -d ':' $states $raw > $map
rm -f $dir/*.$ext
while read -r line; do
  state=$(echo $line | sed -n 's/\(.*\):.*/\1/p')
  if test "$concatenate" = "yes"; then
    filename="$dir/all.$ext"
  else
    filename=$(printf "$dir/%04d.$ext" $state)
  fi
  sentence=$(echo $line | sed -n 's/.*:\(.*\)/\1/p')
  echo $sentence | $unlexer >> $filename
done < $map
printf "done.\n"
