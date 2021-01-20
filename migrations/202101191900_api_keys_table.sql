create schema if not exists account;

CREATE EXTENSION IF NOT EXISTS citext;

CREATE OR REPLACE FUNCTION create_timestamps()   
        RETURNS TRIGGER AS $$
        BEGIN
            NEW.created_at = now();
            NEW.updated_at = now();
            RETURN NEW;   
        END;
        $$ language 'plpgsql';

CREATE OR REPLACE FUNCTION update_timestamps()   
        RETURNS TRIGGER AS $$
        BEGIN
            NEW.updated_at = now();
            RETURN NEW;   
        END;
        $$ language 'plpgsql';


create table if not exists account.api_key
  (
    id bigint generated always as identity primary key,
    key citext unique not null,
    owner_email citext unique not null,
    owner_name text not null,
    is_enabled boolean not null,
    created_at timestamp with time zone default current_timestamp not null,
    updated_at timestamp with time zone default current_timestamp not null
  );

drop trigger if exists account_api_key_insert on account.api_key;
  create trigger account_api_key_insert before insert on account.api_key for each row execute procedure create_timestamps();
drop trigger if exists account_api_key_update on account.api_key;
  create trigger account_api_key_update before update on account.api_key for each row execute procedure update_timestamps();


create index if not exists idx_enabled_key on account.api_key(key, is_enabled);
