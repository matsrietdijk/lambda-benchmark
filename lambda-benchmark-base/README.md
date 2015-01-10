# Database

## Generate dummy content

```fish
set lorem (curl http://www.loripsum.net/api/4); \
for i in (seq 20); \
    psql 'lambda-benchmark' -c "insert into employee (title, content, firstname, lastname, created_at) \
    values ('Employee $i', '$lorem', 'Voornaam', 'Achternaam', current_date);";
end;
set lorem (curl http://www.loripsum.net/api/6); \
for i in (seq 15); \
    psql 'lambda-benchmark' -c "insert into project (title, content, url, customer, created_at) \
    values ('Project $i', '$lorem', 'http://dummy.url', 'Klant', current_date);";
end
```
