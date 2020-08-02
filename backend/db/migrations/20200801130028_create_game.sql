-- migrate:up

create table games
(
    id            serial,
    name          varchar not null,
    created_at    timestamptz not null default now(),
    updated_at    timestamptz not null default now()
);

create unique index games_id on games (id);

CREATE TRIGGER games_updated_at
    BEFORE UPDATE ON games
    FOR EACH ROW
EXECUTE PROCEDURE update_timestamp();

-- migrate:down
drop table games;
