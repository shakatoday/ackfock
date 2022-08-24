SELECT DISTINCT ON (user_ackfock.user_id) *
  FROM user_ackfock
       INNER JOIN users ON users.uuid = user_ackfock.user_id
 WHERE memo_id = ?
 ORDER BY user_ackfock.user_id, user_ackfock.created_at DESC
