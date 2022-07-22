/*
  This is a sanitized version of the function we are using. In essence, it intercepts calls to db.run()
  and, based on the type of query, either just does the query, or instead, adds another call transactionally
  with that query. However, we are seeing that even some selects, by this function, call the database procedure
  transactionally, and in testing, were showing up as having Effect of Write. More context and sanitized debug 
  traces are available in the txt file in this repo.
*/

def run[A, E <: Effect: TypeTag](a: DBIOAction[A, NoStream, E]): Future[A] = {
 typeOf[E] match {
   case r if r =:= typeOf[Read] => underlyingDB.run(a)
   case t if t =:= typeOf[Transactional] => {
     if (isUpdateOrInsert()) {
       underlyingDB
         .run((a zip databaseProcRepository.callDatabaseProc().transactionally)
         .map(tup => {
           tup._1
         })
     } else {
       underlyingDB.run(a)
     }
   }
   case _ =>
     underlyingDB
       .run((a zip databaseProcRepository.callDatabaseProc().transactionally)
       .map(_._1)
 }
