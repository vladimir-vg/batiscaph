-- :select_instances_infos_with_ids
SELECT InstanceId, Type, (toUInt64(AtSec)*1000*1000 + AtMcs) AS At
FROM `:dbname`.events
WHERE InstanceId IN :ids
  AND Type IN ('vision 0 connection-start', 'vision 0 connection-stop')
LIMIT 100
FORMAT TabSeparatedWithNamesAndTypes;
