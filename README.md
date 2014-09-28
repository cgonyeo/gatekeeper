Gatekeeper
==========

This is a remake of an existing project at the Computer Science House also
called Gatekeeper. Gatekeeper is a service that provides our members access to
specific rooms through their iButtons. Doors are equipped with readers, and are
set to disable the lock for a few seconds when a valid iButton is presented to
a door.

This implementation aims to make several improvements over the current
implementation. Specifically the improvements are:

- This is designed to use RFID instead of iButtons
- Has a local cache of all RFID -> user mappings, and is thus resilient to network
  downtime
- Has no central server to depend on, besides LDAP
- When a door discovers a new RFID tag addition or deletion, it is shared with
  all other doors, thus minimizing communication with the LDAP server

The project is written in Haskell, and the local cache is implemented using an
Observed-Removed Set CRDT, which is defined in
[this](http://hal.inria.fr/docs/00/55/55/88/PDF/techreport.pdf) paper. All nodes
check the LDAP server for changes, at random intervals and independent of each
other.

This project is intended to run on a [BeagleBone
Black](http://beagleboard.org/black), and use an RFID reader from
[Sparkfun](https://www.sparkfun.com/tutorials/243).

A non-exhaustive list of features and behaviors, only some of which are
currently implemented:

- With the exception of LDAP, the service is fully distributed and not reliant
  on any server.
- New services that wish to use RFID tags for authentication could just join the
  CRDT cluster, and receive updates to the set for free. I intend to provide a
  library written in Haskell that exposes C binding to facilitate exactly this.
- By nature of the properties of a CRDT, all operations are local and atomic,
  and therefore immediate, and reconciling differences between doors is easy.
- When an individual node is unable to reach the rest of the nodes for updates,
  it will stop allowing access to a room after some timeout period. If a network
  cable gets borrowed for 5 minutes service will be uninterrupted, and if
  someone disconnects a door with malicious intent there will not be a security
  hole for a large amount of time.
- Periodic heartbeats to other nodes will make sure all nodes are online, and
  the cluster will send emails if a door is offline for a significant period of
  time.
- Periodic heartbeats will include hashes of the entire data set, and a merkle
  tree will be used to pinpoint the missing data.
- SSL connections and pre-agreed upon secrets will be utilized to prevent
  snooping of any of the RFID tags or member usernames.
- Each door will host a site providing access logs for all doors, and controls to
  temporarily or permanently lock or unlock any given door. Traffic to the site
  will be load balanced across doors.
- The weak consistency guarantees of a CRDT results in changes not being
  guaranteed to be immediate (although in practice with this implementation and
  CSH's network they will probably always appear to be, barring any network
  partitions). This is accounted for with the heartbeats and timeouts.
- One possible behavior could be detecting when the LDAP server is unavailable,
  and exposing controls on the site to add or remove RFID tags for specific
  users to remove dependency on the LDAP server. All changes made during this
  period would probably only be temporary until the LDAP server became available
  again, so I can avoid dealing with writing to LDAP.
