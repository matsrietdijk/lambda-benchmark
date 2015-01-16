# Setup

## Create db

```bash
echo " \
  create database lambda_benchmark; \
  grant all privileges on lambda_benchmark.* to \
  lambda_benchmark@localhost identified by 'lambda_benchmark'; " \
  | mysql -uroot -p
```

## install yesod app

run the following commands:

```fish
cd lambda-benchmark-project; and cabal install

cd ../lambda-benchmark-employee; and cabal install

cd ../lambda-benchmark-base; and cabal install; and yesod devel
```

## Generate dummy content

```fish
for l in 'en' 'nl'; \
    set lorem (curl http://www.loripsum.net/api/4); \
    for i in (seq 20); \
        echo "insert into lambda_benchmark.employee (ident, title, content, firstname, lastname, lang) \
        values ('$i', 'Employee $i', '$lorem', 'Voornaam', 'Achternaam', '$l');" \
        | mysql -ulambda_benchmark -plambda_benchmark; \
    end; \
    set lorem (curl http://www.loripsum.net/api/6); \
    for i in (seq 15); \
        echo "insert into lambda_benchmark.project (ident, title, content, url, customer, lang) \
        values ('$i', 'Project $i', '$lorem', 'http://dummy.url', 'Klant', '$l');" \
        | mysql -ulambda_benchmark -plambda_benchmark; \
    end; \
end
```
