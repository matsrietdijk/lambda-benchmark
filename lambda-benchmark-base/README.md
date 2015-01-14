# Setup

## Create db

```bash
echo " \
  create database 'lambda-benchmark'; \
  create user 'lambda-benchmark' with password 'lambda-benchmark'; \
  grant all on 'lambda-benchmark' to 'lambda-benchmark';" \
  | psql
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
        psql 'lambda-benchmark' -c "insert into employee (ident, title, content, firstname, lastname, lang, created_at) \
        values ('$i', 'Employee $i', '$lorem', 'Voornaam', 'Achternaam', '$l', current_date);"; \
    end; \
    set lorem (curl http://www.loripsum.net/api/6); \
    for i in (seq 15); \
        psql 'lambda-benchmark' -c "insert into project (ident, title, content, url, customer, lang, created_at) \
        values ('$i', 'Project $i', '$lorem', 'http://dummy.url', 'Klant', '$l', current_date);"; \
    end; \
end
```
