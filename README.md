This repository provides shared functionality, template solutions, documentation, and a common vocabulary, related to, and for applications adhering to, the Lambdatrade Reference Architecture (LRA).

LRA currently assumes Haskell, GHC, Cabal, Persistent, PostgreSQL, and REST APIs.

Copyright © 2014-2015 Lambdatrade AB. All rights reserved.

# Using the App Monad

Many applications need to access a database and keep track of application
state. The App monad provides both of this in a neat package.

## Type Parameters
The App monad has 3 type paramaters:

* __st__: The application/user state type. This would normally be a record type
  containing the applications global data (or () if you don't need to track state)
* __r__: The privilege level of the action.
* __l__: The transaction level

### Privilege levels

Actions are divided in privileged and unprivileged actions. Unprivileged actions
generall only perform read-only operations (except for logging), while
privileged actions can also update the database.

Unprivileged actions can be run in a privileged context by using the
`unprivileged` function

At the moment it is the responsibility of the user to set the appropriate
privilege level of actions.

### Transaction levels

Postegresql can operate in 3 transaction levels that provide different
separation guarantees:

* Read committed
* Repeatable Read
* Serializeable

For in-depth discussion of the semantics of those levels please refer to the
[Postgres documentation](https://www.postgresql.org/docs/9.5/static/transaction-iso.html).

The app monad keeps track of the _minimum required_ transaction level for an action.

Use `withReadCommitted`, `withRepeatableRead` and `withSerializeable` to set /
upgrade the required transaction level of an action. Note that the transaction
level can't be downgraded. `runApp'` will automatically set the necessary level
in a new transaction before running the action. This only works if you set the level using the aforementioned functions.

## Talking to the database

The App monad works with the database connectivitiy provided by the persistent package.
To run a database action, lift it into App using the `db` function for privileged actions (read-only or publically modifyable state) and `db'` for privileged ones

## Retrieving application state

To grab the application state you set in `runApp'`, use `aslState` or
`viewState`. The latter allows you to pass a lens to retrieve the part that
interests you

## Working with the App type

Having to write out the 3 type parameters becomes tedious quickly. Therefore, we
recommend creating type synonyms to reduce the boiler plate.

For example, suppose your application uses the `ApplicationState` data type to
keep all the global state and doesn't care about privilege levels, you would
define

```haskell
type MyApp tlevel a = App ApplicationState 'Privileged tlevel a
```

And would henceforth use e.g. `MyApp 'ReadCommitted Bool` instead of `App
ApplicationState 'Privileged 'ReadCommitted Bool`

# Logging

## Emitting log messages

Nejla-common provides functionality for easy and consistent logging that is
easily shippable to Elasticsearch via Logstash.

To get started, define your event types either as a single type with multiple
constructors, on for each event you want to log, or one type for each
event. Then make sure they are instances of the `ToJSON` type class (either by
writing an instance by hand or by using aesons generic methods.)

Next you also need to write `LogMessage` instances that gives the type of each
event.

#### Example:

```haskell
data Event = Event { eventDetail1 :: !Text
                   , eventDetail2 :: !Int
                   }

deriveJSON defaultOptions ''Event

instance LogMessage Event where
  messageType Event{} = "my_event_type"
```

Now you can use `toLogRow` to construct a log row for custom logging or use
`logEvent` to log an event to stderr in json format


#### Example:

```haskell
logEvent Event{ eventDetail1 = "foo"
              , eventDetail2 = 77
              }
```

would produce

```json
{"eventDetail1":"foo","time":"2016-08-11T13:23:30.637977Z","eventDetail2":77,"type":"my_event_type"}
```

## Shipping to Elastic Stack

In a production environment you will probably want to ship the logs to a central
loggin facility like the ELK stack.

First you need to set up the ELK (now "elastic") stack. For this, please refer
to the elastic documentation.

Next you will have to configure logstash to accept logs via the GELF protocol,
so add the following the the logstash.conf

```
input {
  gelf {
    port => 12201
  }
}
```
replacing to the port number as desired.

You will also have to set up processing of the log rows:

```
filter {
  json {
    source => "message"
    add_field => {"parsed_json" => true }
    remove_field => ["message", "time"]
  }
}
```

and shipping to elasticsearch:

```
output {
    elasticsearch {
      hosts => ["http://elasticsearch:9200/"]
      index => "logstash-%{instance}"
    }
}
```

again, replacing the hostname and port as needed, and restart logstash.

Now you can set up docker to ship its rows to logstash. To do so, (re-)create
your containers with the following option

`--log-driver=gelf --log-opt gelf-address=udp://host:port` replacing "host" and
"port" with the hostname and port of the logstash instance (the port you
selected in the previous section)

or if you use docker-compose (compose-file version 2), add the following to the
service sections:

```
    logging:
      driver: gelf
      options:
        gelf-address: udp://host:port
```

Logs should now be shipped to your ES instance.

#### Complete example of logstash.conf

This example includes more complex processing.
* It adds handling of log messages of the form `[LOGLEVEL#component] message` as
  produced by persistent
* It enables geoip resolution of IP addresses stored in the "ip" field

For an in-depth discussion of logstash configuration, please refer to the [logstash documentation](https://www.elastic.co/guide/en/logstash/current/configuration.html)

```
input {
  gelf {
    port => 12201

  }
}

filter {
  if [message] =~ /\A{.*}\Z/ {
    json {
      source => "message"
      add_field => {"parsed_json" => true }
      remove_field => ["message", "time"]
    }
  }
  else {
    grok {
      match => {message =>
        [ "\A\[%{LOGLEVEL:level}(#(?<component>[^\]]+))?]%{GREEDYDATA:message}\Z"
        ]}
      overwrite => ["message"]
      add_field => ["type", "logs"]
    }

  }

  if [ip] {
    geoip {
      source => "ip"
      target => "geoip"
      add_field => [ "[geoip][coordinates]", "%{[geoip][longitude]}" ]
      add_field => [ "[geoip][coordinates]", "%{[geoip][latitude]}"  ]
    }
    mutate {
      convert => [ "[geoip][coordinates]", "float" ]
    }
  }
}

output {
    elasticsearch {
      hosts => ["http://elasticsearch:9200/"]
      index => "logstash-%{instance}"
    }
}
```

# Warnings

Warnings can catch errors early. To make them useful, projects should be kept
warning-clean during development, so all code should be compiled with
all warnings enabled by adding:

```
  ghc-options:      -Wall
```

to each library, executable and test-suite section.

Warnings should only be disabled when fixing them is infeasible on a by-need
basis. preferably by adding the appropriate pragma to the respective source file:


```
{-# OPTIONS_GHC -fno-warn-orphans -#}
{-# OPTIONS_GHC -fno-warn-type-defaults -#}
```

or disabling them for the whole section if those pragmas would be used in many
source files:

```
  ghc-options:      -Wall -fno-warn-orphans
```

## Likely candidates for suppression
* -fno-warn-orphans
* -fno-warn-type-defaults
