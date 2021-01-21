-- speed up autocomplete: denormalize, and bias towards more populous cities.

create materialized view if not exists geocode.city_autocomplete
  as select geocode.city.geonameid, geocode.city.name,
      location[0] as longitude, location[1] as latitude,
      population, timezone,
      geocode.country.iso as country_code,
      geocode.country.name as country_name,
      geocode.region.name as region_name,
      geocode.district.name as district_name
    from geocode.city
     left join geocode.country using(isocode) 
     left join geocode.region using(isocode, regcode)
     left join geocode.district using(isocode, regcode, discode)
     order by population desc;

-- we could also use a GiST index, but some tinkering in `psql` demonstrate that,
-- at least for this dataset, GIN outperforms it in average usage (i.e. ~3 letter queries.)
create index if not exists idx_autocomplete on geocode.city_autocomplete using gin(name gin_trgm_ops);
