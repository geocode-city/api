begin;

\copy raw.feature from 'featureCodes_en.txt' with csv delimiter E'\t'
\copy raw.admin1 from 'admin1CodesASCII.txt' with csv delimiter E'\t'
\copy raw.admin2 from 'admin2Codes.txt' with csv delimiter E'\t'
\copy raw.country from 'countryInfoData.txt' with csv delimiter E'\t'


insert into geocode.class (class, description)
     values ('A', 'country, state, region,...'),
            ('H', 'stream, lake, ...'),
            ('L', 'parks,area, ...'),
            ('P', 'city, village,...'),
            ('R', 'road, railroad '),
            ('S', 'spot, building, farm'),
            ('T', 'mountain,hill,rock,... '),
            ('U', 'undersea'),
            ('V', 'forest,heath,...');


insert into geocode.feature
     select substring(code from 1 for 1) as class,
            substring(code from 3) as feature,
            description,
            comment
       from raw.feature
      where feature.code <> 'null';            

insert into geocode.continent(code, name)
     values ('AF', 'Africa'),
            ('NA', 'North America'),
            ('OC', 'Oceania'),
            ('AN', 'Antarctica'),
            ('AS', 'Asia'),
            ('EU', 'Europe'),
            ('SA', 'South America');

insert into geocode.country
     select isocode, iso, iso3, fips, name,
            capital, continent, tld, geonameid
       from raw.country;            


insert into geocode.neighbour
   with n as(
     select isocode,
            regexp_split_to_table(neighbours, ',') as neighbour
       from raw.country
   )
   select n.isocode,
          country.isocode
     from n
          join geocode.country
            on country.iso = n.neighbour;

insert into geocode.region
   with admin as
   (
     select regexp_split_to_array(code, '[.]') as code,
            name,
            geonameid
       from raw.admin1
   )
   select country.isocode as isocode,
          code[2] as regcode,
          admin.name,
          admin.geonameid
     from admin
          join geocode.country
            on country.iso = code[1];

insert into geocode.district
   with admin as
   (
     select regexp_split_to_array(code, '[.]') as code,
            name,
            geonameid
       from raw.admin2
   )
     select region.isocode,
            region.regcode,
            code[3],
            admin.name,
            admin.geonameid
       from admin
            
            join geocode.country
              on country.iso = code[1]
            
            join geocode.region
              on region.isocode = country.isocode
             and region.regcode = code[2];

commit;
