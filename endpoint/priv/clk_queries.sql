-- :select_instances_infos_with_ids
SELECT InstanceId, Type, (toUInt64(AtSec)*1000*1000 + AtMcs) AS At
FROM `:dbname`.events
WHERE InstanceId IN :ids
  AND Type IN ('vision 0 connection-start', 'vision 0 connection-stop')
LIMIT 100
FORMAT TabSeparatedWithNamesAndTypes;



-- :select_events_from_now
SELECT (toUInt64(AtSec)*1000*1000 + AtMcs) AS At, SubId, Type, Pid1, Pid2
FROM `:dbname`.events
WHERE InstanceId = ':instance_id'
  AND Type IN :types
  AND AtSec < now()
ORDER BY AtSec, AtMcs, SubId
LIMIT :limit
FORMAT TabSeparatedWithNamesAndTypes;
