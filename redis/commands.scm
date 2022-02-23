;;; (redis commands) --- redis module for Guile.

;; Copyright (C) 2013-2022 Aleix Conchillo Flaque <aconchillo@gmail.com>
;;
;; This file is part of guile-redis.
;;
;; guile-redis is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;;
;; guile-redis is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with guile-redis. If not, see https://www.gnu.org/licenses/.

;;; Commentary:

;; Redis module for Guile

;;; Code:

(define-module (redis commands)
  #:use-module (redis utils)
  #:use-module (srfi srfi-9)
  #:export (create-command
            redis-command?
            redis-cmd-name
            redis-cmd-args
            redis-cmd-reply))

(define-record-type <redis-command>
  (make-command name args reply)
  redis-command?
  (name redis-cmd-name)
  (args redis-cmd-args)
  (reply redis-cmd-reply))

(define* (create-command name #:rest args)
  (make-command name args read-reply))

(define-syntax create-commands
  (syntax-rules ()
    ((_ (cmd ...) ...)
     (eval
      `(begin
         ,@(map
            (lambda (args)
              (apply (lambda* (name #:rest subnames)
                       (let* ((cmd-name (string-join `(,name ,@subnames) " "))
                              (func-name (string->symbol (string-join `(,name ,@subnames) "-"))))
                         `(begin
                            (define* (,func-name #:optional args)
                              (apply create-command ,(string-upcase cmd-name) (or args '())))
                            (module-export! (current-module) '(,func-name)))))
                     args))
            `((,(symbol->string (syntax->datum #'cmd)) ...) ...)))
      (current-module)))))

(create-commands
 ;; Bitmap
 (bitcount)
 (bitfield)
 (bitfield_ro)
 (bitop)
 (bitpos)
 (getbit)
 (setbit)
 ;; Cluster
 (asking)
 (cluster addslots)
 (cluster addslotsrange)
 (cluster bumpepoch)
 (cluster count-failure-reports)
 (cluster countkeysinslot)
 (cluster delslots)
 (cluster delslotsrange)
 (cluster failover)
 (cluster flushslots)
 (cluster forget)
 (cluster getkeysinslot)
 (cluster info)
 (cluster keyslot)
 (cluster links)
 (cluster meet)
 (cluster myid)
 (cluster nodes)
 (cluster replicas)
 (cluster replicate)
 (cluster reset)
 (cluster saveconfig)
 (cluster set-config-epoch)
 (cluster setslot)
 (cluster slaves)
 (cluster slots)
 (readonly)
 (readwrite)
 ;; Connection
 (auth)
 (client caching)
 (client getname)
 (client getredir)
 (client id)
 (client info)
 (client kill)
 (client list)
 (client no-evict)
 (client pause)
 (client reply)
 (client setname)
 (client tracking)
 (client trackinginfo)
 (client unblock)
 (client unpause)
 (echo)
 (hello)
 (ping)
 (quit)
 (reset)
 (select)
 ;; Geo
 (geoadd)
 (geodist)
 (geohash)
 (geopos)
 (georadius)
 (georadiusbymember)
 (georadiusbymember_ro)
 (georadius_ro)
 (geosearch)
 (geosearchstore)
 ;; Hashes
 (hdel)
 (hexists)
 (hget)
 (hgetall)
 (hincrby)
 (hincrbyfloat)
 (hkeys)
 (hlen)
 (hmget)
 (hmset)
 (hrandfield)
 (hscan)
 (hset)
 (hsetnx)
 (hstrlen)
 (hvals)
 ;; HyperLogLog
 (pfadd)
 (pfcount)
 (pfdebug)
 (pfmerge)
 (pfselftest)
 ;; Keys
 (copy)
 (del)
 (dump)
 (exists)
 (expire)
 (expireat)
 (expiretime)
 (keys)
 (migrate)
 (move)
 (object encoding)
 (object freq)
 (object idletime)
 (object refcount)
 (persist)
 (pexpire)
 (pexpireat)
 (pexpiretime)
 (pttl)
 (randomkey)
 (rename)
 (renamenx)
 (restore)
 (scan)
 (sort)
 (sort_ro)
 (touch)
 (ttl)
 (type)
 (unlink)
 (wait)
 ;; Lists
 (blmove)
 (blmpop)
 (blpop)
 (brpop)
 (brpoplpush)
 (lindex)
 (linsert)
 (llen)
 (lmove)
 (lmpop)
 (lpop)
 (lpos)
 (lpush)
 (lpushx)
 (lrange)
 (lrem)
 (lset)
 (ltrim)
 (rpop)
 (rpoplpush)
 (rpush)
 (rpushx)
 ;; Pub/Sub
 ;; publish, subscribe, unsubscribe, etc. are defined in pubsub.scm.
 (pubsub channels)
 (pubsub numpat)
 (pubsub numsub)
 (pubsub shardchannels)
 ;; Scripting
 (eval)
 (evalsha)
 (evalsha_ro)
 (eval_ro)
 (fcall)
 (fcall_ro)
 (function delete)
 (function dump)
 (function flush)
 (function kill)
 (function list)
 (function load)
 (function restore)
 (function stats)
 (script debug)
 (script exists)
 (script flush)
 (script kill)
 (script load)
 ;; Server
 (acl cat)
 (acl deluser)
 (acl dryrun)
 (acl genpass)
 (acl getuser)
 (acl list)
 (acl load)
 (acl log)
 (acl save)
 (acl setuser)
 (acl users)
 (acl whoami)
 (bgrewriteaof)
 (bgsave)
 (command)
 (command count)
 (command docs)
 (command getkeys)
 (command getkeysandflags)
 (command info)
 (command list)
 (config get)
 (config resetstat)
 (config rewrite)
 (config set)
 (dbsize)
 (failover)
 (flushall)
 (flushdb)
 (info)
 (lastsave)
 (latency doctor)
 (latency graph)
 (latency histogram)
 (latency history)
 (latency latest)
 (latency reset)
 (lolwut)
 (memory doctor)
 (memory malloc-stats)
 (memory purge)
 (memory stats)
 (memory usage)
 (module list)
 (module load)
 (module unload)
 (monitor)
 (psync)
 (replconf)
 (replicaof)
 (restore-asking)
 (role)
 (save)
 (shutdown)
 (slaveof)
 (slowlog get)
 (slowlog len)
 (slowlog reset)
 (swapdb)
 (sync)
 (time)
 ;; Sets
 (sadd)
 (scard)
 (sdiff)
 (sdiffstore)
 (sinter)
 (sintercard)
 (sinterstore)
 (sismember)
 (smembers)
 (smismember)
 (smove)
 (spop)
 (srandmember)
 (srem)
 (sscan)
 (sunion)
 (sunionstore)
 ;; Sorted Sets
 (bzmpop)
 (bzpopmax)
 (bzpopmin)
 (zadd)
 (zcard)
 (zcount)
 (zdiff)
 (zdiffstore)
 (zincrby)
 (zinter)
 (zintercard)
 (zinterstore)
 (zlexcount)
 (zmpop)
 (zmscore)
 (zpopmax)
 (zpopmin)
 (zrandmember)
 (zrange)
 (zrangebylex)
 (zrangebyscore)
 (zrangestore)
 (zrank)
 (zrem)
 (zremrangebylex)
 (zremrangebyrank)
 (zremrangebyscore)
 (zrevrange)
 (zrevrangebylex)
 (zrevrangebyscore)
 (zrevrank)
 (zscan)
 (zscore)
 (zunion)
 (zunionstore)
 ;; Streams
 (xack)
 (xadd)
 (xautoclaim)
 (xclaim)
 (xdel)
 (xgroup create)
 (xgroup createconsumer)
 (xgroup delconsumer)
 (xgroup destroy)
 (xgroup setid)
 (xinfo consumers)
 (xinfo groups)
 (xinfo stream)
 (xlen)
 (xpending)
 (xrange)
 (xread)
 (xreadgroup)
 (xrevrange)
 (xsetid)
 (xtrim)
 ;; Strings
 (append)
 (decr)
 (decrby)
 (get)
 (getdel)
 (getex)
 (getrange)
 (getset)
 (incr)
 (incrby)
 (incrbyfloat)
 (lcs)
 (mget)
 (mset)
 (msetnx)
 (psetex)
 (set)
 (setex)
 (setnx)
 (setrange)
 (strlen)
 (substr)
 ;; Transactions
 (discard)
 (exec)
 (multi)
 (unwatch)
 (watch))

;;; (redis commands) ends here
