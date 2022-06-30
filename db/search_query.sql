SELECT memo.*, ts_rank(to_tsvector(memo.CONTENT), query) + similarity AS search_rank
  FROM memo,
       to_tsvector(memo.CONTENT) DOCUMENT,
       to_tsquery(?) query,
       similarity(?, memo.CONTENT) similarity
 WHERE query @@ DOCUMENT OR similarity > 0.1
 ORDER BY search_rank DESC
