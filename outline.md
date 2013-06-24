# Social Networking Mashup - Outline

  1. Description of the application: explaining the idea of aggregating
     the feeds from various social networking sites. Quickly go over how
     the application should work from the user's point-of-view; the idea of
     multiple sources and displaying them in reverse-chronological

  2. Taking a look at the data model. How we can easily abstract a generic
     feed item and a feed source. How we can represent this data in Haskell.

     Defining a type class for feed sources, and how we can ensure that all
     instances of that type class satisfy various constraints (such as
     feed items being serialisable).

     We could go into all sorts of details about type classes, type families
     and placing constraints in class headers, etc. I've found that people
     with a programming background quite quickly grasp type classes if explained
     in terms of "interface" and "implementation".

  3. Building a dummy feed source which just pulls items from the database
     so that we can work on the client side. This provides an implementation
     of the type class we defined earlier.

  4. Serialisation of the feed items to the client-side of the application
     as JSON. Taking a look at implementing the `aeson` type classes
     `FromJSON` and `ToJSON`.

     Whilst it would be easy to get bogged down in the explanation of
     applicative functors and monads, it's largely unnecessary to do so
     with the rather simple structures that we'll use here.

  5. User authentication. This application should use JSON-based
     authentication. This seems to be a bit of a sticking point for people
     new to the Haskell+Yesod world.

  6. Defining a RESTful API for the application.

  5. An overview of the client-side code without actually going into the
     details of how it is implemented. The client-side is JavaScript, which
     is not what this tutorial is about.

     The main focus should be on communicating with the application from
     the client; specifically the definition and use of a RESTful API.

     Also some examination of the pagination of data, and how the API can
     be expanded to allow the feed from the application to the client to
     be limited and offset.

  7. Adding a feed source: Facebook. First of all we will take a look at
     the parts of the Facebook API that we are interested in, mostly:

        i) Authentication of the Facebook user.

        ii) Authorising an application inc. the generation of an Application ID.

        iii) Querying the Facebook graph to get the user's friend list.

        iv) Querying each friend for a list of posts (called 'notes').

  8. Adding the Twitter feed source. Much the same as the Facebook source,
     except for Twitter. We won't need to go into as much detail either,
     although some emphasis on how we're just providing a new implementation
     of the type class should be made clear.

  9. Caching of results from the various sources so that we only need to
     ask for the latest results from the sources themselves. This is quite
     a nice example of how the program can be extended quite trivially.

     This expansion can be basically thought of a modifying the interface
     (via a type class). We can then move through the two implementations
     of that type class (the Dummy, Facebook and Twitter sources) and
     modify their implementations.

     Whilst this is a cool idea, and something that I've often seen asked,
     it is perhaps the part of the application that can be sacrified first.

  10. Closing with an conclusion which takes a more abstract look at what
      the reader has achieved:

        i) Defined an easily extensible, caching feed aggregator.

        ii) Developed the application extremely quickly by abstracting
            away almost all complexity.

        iii) Trivially implemented a RESTful API which provides a type-safe
             interface between the client and the server, and the server
             and the feed sources.
