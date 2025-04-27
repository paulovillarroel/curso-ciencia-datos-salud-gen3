select *
from establecimientos_db;

select count(*)
from establecimientos_db;

select RegionGlosa, count(*) as total_establecimientos
from establecimientos_db
group by RegionGlosa
order by total_establecimientos desc;
