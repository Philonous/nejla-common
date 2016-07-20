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
