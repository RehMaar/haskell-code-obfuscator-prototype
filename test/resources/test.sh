#!/bin/bash

elements=$@

RUNGHC="stack runghc"
EXEC="stack exec -- obfuscate-exe"

for file in $elements
do
    echo $file
    orig_result_file=$(mktemp)
    obfuscated_content_file=$(mktemp)
    obfuscated_result_file=$(mktemp)
    $RUNGHC "$file" > $orig_result_file
    $EXEC "$file" > $obfuscated_content_file
    $RUNGHC "$obfuscated_content_file" > "$obfuscated_result_file"
    # -y for showing results in two columns
    diff -q "$orig_result_file" "$obfuscated_result_file"
    rm "$orig_result_file" "$obfuscated_content_file" "$obfuscated_result_file"
done
