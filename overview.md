# FP Complete: Example Yesod App

Provide a social-networking "mash-up" application that demonstrates the Yesod
framework.

## The Application

The application should provide a "mash-up" of feeds from social networking
sites. As this is just an example application, it should just use FaceBook and
Twitter for the time being.

The application itself (on the server side) is more an aggregation service,
that takes the feed items from a user's Facebook friends and people followed
on Twitter and aggregates these into a standard representation that is sent
to the client. Additionally, the server caches th results from the social
network sources to reduce bandwidth.

The front-end is a simple JavaScript application that queries the server for the
feed items and displays them in a grid. The client will also paginate the display
with an "infinite scroll".

The application is built on top of the Yesod scaffold, with a view to creating
a new scaffolding template that includes insertion points for inserting the
definitions outlined in the tutorial.

## Haskell Features

The application is intended to demonstrate a number of Haskell's features,
without placing too high a demand on the user in terms of experience with the
Haskell language.

### JSON Serialisation

The application uses JSON serialisation quite a lot in both the communication
with social networking sites and the communication between the client and the
application.

Whilst the standard library for JSON serialisation (aeson) requires an
understanding of applicative functors, I feel that this is a fairly intuitive
process that can be understood by someone with only a passing understanding of
Haskell.

### Type Classes and Type Families

These two concepts often come together quite nicely, and can be demonstrated
quite easily in this application as follows:

Each social networking site that is queried by the system provides an instance
of a `SocialFeed` type class which has an associated `FeedItem` family type,
for example:

    class SocialFeed site where
      data FeedItem site

      getFeedItems :: site -> [FeedItem site]

The actual `SocialFeed` type class will have to be slightly more complex than
this to support the different sources from which `FeedItem`s can be obtained,
e.g. friends in Facebook's social graph and people followed in Twitter's API.

To ensure that 'FeedItem's and so on can be serialised to and from JSON, we can
put a constraint in the class header:

    class (ToJSON (FeedItem site)) => SocialFeed site where
      ...

## The Tutorial

The tutorial is to be written for FP Complete's School of Haskell. The tutorial
does not need to dive into the details of monad transformers, applicative functors
and so on. References to pre-existing documentation can be made (for example,
Michael Snoyman's Yesod book or other articles on the School or Haskell).

