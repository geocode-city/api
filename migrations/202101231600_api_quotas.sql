alter table account.api_key
  add column if not exists monthly_quota bigint default 100000;
