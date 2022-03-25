# TODO: find a better way to store or chain data to the final output
# converts the output to a csv
sexp query each | sexp to-csv | \

# 'cleans' the output by grabbing only the colummns we care about
csvcut -c full_benchmark_name,time_per_run_nanos,minor_words_per_run,major_words_per_run,promoted_words_per_run | \
sed 's/execute//' | sed 's/compile\svalue//' # further cleans the csv by removing extraneous data 






