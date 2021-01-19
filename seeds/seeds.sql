begin;

insert into geocode.city
  with geo as
  (
     select geonameid,
            name,
            point(longitude, latitude) as location,
            country_code,
            admin1_code,
            admin2_code,
            feature_class,
            feature_code,
            population,
            elevation,
            timezone,
            alternatenames
       from raw.geonames
   )
     select geo.geonameid,
            geo.name,
            geo.location,
            country.isocode,
            region.regcode,
            district.discode,
            feature.class,
            feature.feature,
            population,
            elevation,
            timezone,
            alternatenames
       from geo
            left join geocode.country
              on country.iso = geo.country_code

            left join geocode.region
              on region.isocode = country.isocode
             and region.regcode = geo.admin1_code

            left join geocode.district
              on district.isocode = country.isocode
             and district.regcode = geo.admin1_code
             and district.discode = geo.admin2_code

           left join geocode.feature
             on feature.class = geo.feature_class
            and feature.feature = geo.feature_code;

commit;            
