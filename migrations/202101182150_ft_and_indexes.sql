begin;

-- this would apply English stemming and such that can lead to false negatives for foreign city names:
--CREATE INDEX IF NOT EXISTS idx_city_ftsearch ON geocode.city USING GIN (to_tsvector('english', name || ' ' || alternatenames));
CREATE INDEX IF NOT EXISTS idx_city_name on geocode.city(name);
CREATE INDEX IF NOT EXISTS idx_location on geocode.city using GIST(location);

-- we use trigrams instead of full text search since city names
-- don't lend themselves well for partial matches when approached as words
-- (e.g. searching for `tegu` doesn't come up with tegucigalpa when using the `english` pg dictionary)
create extension pg_trgm;
create index if not exists idx_city_autocomplete on geocode.city using gist(alternatenames gist_trgm_ops);
-- we could also create an index on name, which may lose some results, but is 3x faster:
create index if not exists idx_city_autocomplete_fast on geocode.city using gist(name gist_trgm_ops);

commit;