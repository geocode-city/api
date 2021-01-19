create schema if not exists geocode;

create table if not exists geocode.class
 (
  class        char(1) not null primary key,
  description  text
 );

create table if not exists geocode.feature
 (
  class       char(1) not null references geocode.class(class),
  feature     text    not null,
  description text,
  comment     text,

  primary key(class, feature)
 );

create table if not exists geocode.continent
 (
  code    char(2) primary key,
  name    text
 );

create table if not exists geocode.country
 (
  isocode   integer primary key,
  iso       char(2) not null,
  iso3      char(3) not null,
  fips      text,
  name      text,
  capital   text,
  continent char(2) references geocode.continent(code),
  tld       text,
  geonameid bigint
 );


create table if not exists geocode.neighbour
 (
  isocode   integer not null references geocode.country(isocode),
  neighbour integer not null references geocode.country(isocode),

  primary key(isocode, neighbour)
 );

create table if not exists geocode.region
 (
  isocode   integer not null references geocode.country(isocode),
  regcode   text not null,
  name      text,
  geonameid bigint,

  primary key(isocode, regcode)
 );

create table if not exists geocode.district
 (
  isocode   integer not null,
  regcode   text not null,
  discode   text not null,
  name      text,
  geonameid bigint,

  primary key(isocode, regcode, discode),
  foreign key(isocode, regcode)
   references geocode.region(isocode, regcode)
 );
