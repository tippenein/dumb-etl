dumb-etl
====

A starting point for ETL scripting

```
ETL Shtuff

Usage: dumb-etl --input STRING [--output STRING] --search STRING
                --pattern STRING [--matcher MATCHER]

Available options:
  -h,--help                Show this help text
```


For example, running a simple (but fast) indice search on `tweets/tweets_*` and dumping the results to the default output (`results.txt`)
```
stack exec dumb-etl -- --input tweets/ \
                       --search 'knicks' \
                       --pattern 'tweets_*' \
```

Using a regex pattern match to dump more detailed results to a different output file:
```
stack exec dumb-etl -- --input tweets/ \
                       --matcher Pattern \
                       --search 'kni*' \
                       --pattern 'tweets_*' \
                       --output neighborhoods.txt
```
