create table if not exists geocode.city
  (
    geonameid bigint primary key,
    name text,
    location point,
    isocode integer,
    regcode text,
    discode text,
    class char(1),
    feature text,
    population bigint,
    elevation bigint,
    timezone text,
    alternatenames text,

   foreign key(isocode)
    references geocode.country(isocode),
   
   foreign key(isocode, regcode)
    references geocode.region(isocode, regcode),
    
   foreign key(isocode, regcode, discode)
    references geocode.district(isocode, regcode, discode),

   foreign key(class)
    references geocode.class(class),

   foreign key(class, feature)
    references geocode.feature(class, feature)    
  );
