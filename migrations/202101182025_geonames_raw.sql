create schema if not exists raw;

-- main cities table, as per:
-- http://download.geonames.org/export/dump/readme.txt
create table if not exists raw.geonames
 (
   geonameid         bigint,
   name              text,
   asciiname         text,
   alternatenames    text,
   latitude          double precision,
   longitude         double precision,
   feature_class     text,
   feature_code      text,
   country_code      text,
   cc2               text,
   admin1_code       text,
   admin2_code       text,
   admin3_code       text,
   admin4_code       text,
   population        bigint,
   elevation         bigint,
   dem               bigint,
   timezone          text,
   modification      date
 );

create table if not exists raw.country
 (
  iso                 text,
  iso3                text,
  isocode             integer,
  fips                text,
  name                text,
  capital             text,
  area                double precision,
  population          bigint,
  continent           text,
  tld                 text,
  currency_code       text,
  currency_name       text,
  phone               text,
  postal_code_format  text,
  postal_code_regex   text,
  languages           text,
  geonameid           bigint,
  neighbours          text,
  fips_equiv          text
 );


create table if not exists raw.feature
 (
  code        text,
  description text,
  comment     text
 );


create table if not exists raw.admin1
 (
  code       text,
  name       text,
  ascii_name text,
  geonameid  bigint
 );


create table if not exists raw.admin2
 (
  code       text,
  name       text,
  ascii_name text,
  geonameid  bigint
 );
