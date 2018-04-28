-- helpers:
--
-- :selectable_attr
if(indexOf(Attrs.Key, ':attr') != 0, arrayElement(Attrs.Value, indexOf(Attrs.Key, ':attr')), '') AS :attr



-- :select_instances_infos_with_ids
SELECT InstanceId, Type, (toUInt64(AtSec)*1000*1000 + AtMcs) AS At
FROM `:dbname`.events
WHERE InstanceId IN :ids
  AND Type IN ('0 batiscaph connection-start', '0 batiscaph connection-stop')
LIMIT 100
FORMAT TabSeparatedWithNamesAndTypes;



-- :select_events_from_now
SELECT (toUInt64(AtSec)*1000*1000 + AtMcs) AS At, SubId, Type, Pid1, Pid2, :attrs
FROM `:dbname`.events
WHERE InstanceId = ':instance_id'
  AND Type IN :types
  AND AtSec < now()
ORDER BY AtSec, AtMcs, SubId
LIMIT :limit
FORMAT TabSeparatedWithNamesAndTypes;



-- :select_request_events
SELECT (toUInt64(AtSec)*1000*1000 + AtMcs) AS At, Type, Pid1, :attrs
FROM `:dbname`.events
WHERE InstanceId = ':instance_id'
  AND Pid1 = ':pid'
  AND Type IN :types
  AND At >= :started_at
  AND At <= :stopped_at
ORDER BY AtSec, AtMcs, SubId
FORMAT TabSeparatedWithNamesAndTypes;
