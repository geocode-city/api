drop materialized view if exists geocode.city_autocomplete;

-- we're using a precalculated to_tsvector column to pay some
-- storage in return for some performance (the vector doesn't
-- have to be calculated in every query.)
-- more about weights and ranks:
-- https://www.postgresql.org/docs/12/textsearch-controls.html#TEXTSEARCH-RANKING
-- https://rob.conery.io/2019/10/29/fine-tuning-full-text-search-with-postgresql-12/
create materialized view if not exists geocode.city_autocomplete
  as select geocode.city.geonameid, geocode.city.name,
      location[0] as longitude, location[1] as latitude,
      population, timezone,
      geocode.country.iso as country_code,
      geocode.country.name as country_name,
      geocode.region.name as region_name,
      geocode.district.name as district_name,
      (
        setweight(to_tsvector('simple', geocode.city.name), 'A') || ' ' ||
        setweight(to_tsvector(coalesce(geocode.district.name, '')), 'C') || ' ' ||
        setweight(to_tsvector(coalesce(geocode.region.name, '')), 'C') || ' ' ||
        setweight(to_tsvector(coalesce(geocode.country.iso, '')), 'B') || ' ' ||
        setweight(to_tsvector(coalesce(geocode.country.name, '')), 'B') || ' ' ||
        setweight(to_tsvector(replace(coalesce(alternatenames, ''), ',', ' ')), 'D')
      ) as autocomplete_doc
    from geocode.city
     left join geocode.country using(isocode) 
     left join geocode.region using(isocode, regcode)
     left join geocode.district using(isocode, regcode, discode);

-- cf. https://www.postgresql.org/docs/12/textsearch-tables.html
create index if not exists idx_full_text_autocomplete
  on geocode.city_autocomplete using gin(autocomplete_doc);

-- cf. https://www.postgresql.org/docs/12/sql-createfunction.html
create or replace function web_to_tsquery_prefix(text) returns tsquery
  as 'select to_tsquery(quote_literal($1) || '':*'');'
  language sql
  immutable
  returns null on null input;
