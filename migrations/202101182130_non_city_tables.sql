
drop schema if exists geocode cascade;
create schema geocode;

create table geocode.class
 (
  class        char(1) not null primary key,
  description  text
 );

create table geocode.feature
 (
  class       char(1) not null references geocode.class(class),
  feature     text    not null,
  description text,
  comment     text,

  primary key(class, feature)
 );

create table geocode.continent
 (
  code    char(2) primary key,
  name    text
 );

create table geocode.country
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


create table geocode.neighbour
 (
  isocode   integer not null references geocode.country(isocode),
  neighbour integer not null references geocode.country(isocode),

  primary key(isocode, neighbour)
 );

create table geocode.region
 (
  isocode   integer not null references geocode.country(isocode),
  regcode   text not null,
  name      text,
  geonameid bigint,

  primary key(isocode, regcode)
 );

create table geocode.district
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
