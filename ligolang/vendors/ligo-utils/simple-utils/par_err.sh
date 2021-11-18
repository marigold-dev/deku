#!/bin/sh

# This script calls Menhir with a message file, which generates the
# corresponding OCaml file.

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

display () {
  printf "\033[31m"; cat $1; printf "\033[0m"
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
    --out=*)
      if test -n "$out"; then
        fatal_error "Repeated option --out."; fi
      out=$(expr "$1" : "[^=]*=\(.*\)")
      ;;
    --out)
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
Usage: $(basename $0) [-h|--help]
                --par-tokens=<par_tokens>.mly
                --lex-tokens=<lex_tokens>.mli
                --out=<par_err>.ml
                <parser>.mly

Generates <par_err>.ml from <parser>.msg and the parser specification
(see messages.sh) in the current directory.

The following options, if given, must be given only once.

Display control:
  -h, --help  display this help and exit

Mandatory options:
      --lex-tokens=<name>.mli the lexical tokens
      --par-tokens=<name>.mly the syntactical tokens
      --out=<par_err>.ml
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

if test -z "$parser"; then
  fatal_error "No parser specification."; fi

if test -z "$par_tokens"; then
  fatal_error "No syntactical tokens specification (use --par-tokens)."; fi

if test -z "$lex_tokens"; then
  fatal_error "No lexical tokens specification (use --lex-tokens)."; fi

if test ! -e "$parser"; then
  fatal_error "Parser specification \"$parser\" not found."; fi

if test ! -e "$lex_tokens"; then
  fatal_error "Lexical tokens specification \"$lex_tokens\" not found."; fi

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

# Checking the presence of the messages

msg=$parser_base.msg
if test ! -e $msg; then
  fatal_error "File $msg not found."; fi

# Checking the output file

if test -z "$out"; then
  fatal_error "Output file missing (use --out)."; fi

# ====================================================================
# Menhir's flags

flags="--table --strict --external-tokens $lex_tokens_base \
       --base $parser_base $par_tokens"

# ===================================================================
# Generating source code from error messages

err=.$msg.err

printf "Making $out from $msg... "
menhir --compile-errors $msg $flags $mly > $out 2> $err
if test "$?" = "0"
then printf "done.\n"
     rm -f $err
else failed ":"
     display "$err"
fi
