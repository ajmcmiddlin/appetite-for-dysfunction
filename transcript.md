---
title: Appetite for dysfunction transcript
---

## Introduction

Hi, my name is Andrew McMiddlin. I'm the programmer formerly known as Andrew McCluskey, and I work
at Data61's Queensland Functional Programming Lab.

Today I'm going to tell you all about how I used functional programming tools to test WordPress;
the blogging and content management system built on PHP and MySQL. Before I get into that though, I
want to give you all a little context on how and why I got here.

To start with, let's take a very quick look at property based testing. Property based testing is a
method of testing whereby inputs to a function are generated randomly, and certain invariants are
checked against the outputs produced by the function being tested. This is in contrast to
traditional unit, or case based, testing, where specific inputs are chosen by the programmer and the
function's output compared to an expected value.

In the Haskell world, this approach was brought to life by Claessen and Hughes in 2000 with
[QuickCheck](http://www.cse.chalmers.se/~rjmh/QuickCheck/). However, I tend to use a relative
newcomer to the property testing world called `hedgehog`. I won't go into the differences between
the two, but if you're interested, `hedgehog`'s author (Jacob Stanley) gave an excellent talk at
Lambda Jam in 2017 that answers this question and serves as an introduction to the library. The talk
is called _Gens and Roses: Appetite for reduction_. This talk's name is a riff on that talk's title.

In `hedgehog`, a property test to ensure that reversing a list twice is the same as doing nothing
looks like this:

```haskell
-- Reverse is involutive
propReverse :: Property
propReverse =
  property $ do
    xs <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
    reverse (reverse xs) === xs
```

As we can see, this code is first generating some random data --- a list of characters with a length
between 0 and 100 --- and then ensuring that the randomly generated list does not violate the stated
property.

Property based testing is a very powerful technique, as our tests are no longer limited to the
examples that we can think of and can be bothered writing. We can now express our tests in terms of
every possible value our function might get, and let a machine execute as many cases as we can be
bothered waiting for. It also forces us to think about the software we're writing in terms of every
possible scenario, rather than focusing on specific examples.

## State machine testing

While very powerful, and very well suited to testing referentially transparent functions, property
based testing in this form is less well equipped to test the higher level properties of stateful
systems. We might write our web application as a referentially transparent expression, but there are
a whole slew of properties that involve the system's state and interactions with the outside world
when the software is running. Just as Claessen and Hughes brought property based testing to Haskell
in 2000 with QuickCheck, they then went on to deliver a solution to this problem in their 2002 paper
on monadic QuickCheck. Again following in QuickCheck's footsteps, `hedgehog` has an implementation
of property based state machine testing that greatly simplifies testing stateful software.

In short, this approach comprises the following steps:

1. Model the application's state with a Haskell data type.
2. Model the actions that might alter the application state.
3. Randomly generate a list of actions with randomly generated input data.
4. Run these actions against the application.
5. Update the model state with the expected changes.
6. Ensure that the model always agrees with the application.

I gave a talk earlier this year at Lambda Jam that aimed to introduce the concept of state machine
testing and how to do it in `hedgehog`. In the talk I covered how `hedgehog`'s state machine testing
facilities work, and provided examples that test part of a Haskell application. This talk is a
follow up in response to questions I received after that talk. Most notably: is it possible to use
hedgehog to test software _not_ written in Haskell. The answer to this question is that it most
definitely can, and to really prove the point, I decided to test WordPress. Using state machine
testing with WordPress uncovered a few interesting challenges that I didn't encounter testing a
Haskell application, so really the focus of this talk is the tools and techniques I used to test
WordPress and not so much the details of the tests.

## WordPress

This is what WordPress say about themselves.

> WordPress is open source software you can use to create a beautiful website, blog, or app.

> WordPress is both free and priceless at the same time.

> 31% of the web uses WordPress, from hobby blogs to the biggest news sites online.

> Over 60 million people have chosen WordPress to power the place on the web they call “home” — join the family.

In short, WordPress is blogging, or web content management software. Given its prevalence, I'm
guessing most people here have at least heard of it. I actually have a couple of old WordPress blogs
lying around, and my assessment of WordPress isn't quite as positive as their marketing material's.
Let's just say that I will be very glad to find the time to move them to Hakyll.

WordPress was started in 2003 as a fork from another project called b2 that was started in 2001. It
is currently 2018, which means that, including its time as b2, the project has been active for about
17 years. It's written in PHP and MySQL, and has a large number of plugins and themes available for
it. Given it's history and technical foundations, you might think that WordPress isn't the most
robust or easy to modify software, and if you did, I would say you are right. To be fair though, I
still find it surprising that it functions as well as it does.

## Let there be... WordPress

Step zero of testing existing software is being able to run the software. Being a paid up member of
the Church of Lambda, I'd like my deployments to be referentially transparent too please. To achieve
this goal I used NixOps. NixOps is the devops spinoff from Nix, where Nix is a purely functional
package manager that uses referentially transparent expressions to build software. As a result, I
have a referentially transparent expression representing everything I need to setup a working
WordPress installation. It gets even better. Nix has a huge library of expressions for all sorts of
software, including WordPress and its dependencies, which means that I had to do very little work.
Here's the code in its entirety.

```nix
{
network.description = "Wordpress";

wordpress =
  { config, pkgs, ... }:
  let
    wpPackage = pkgs.fetchFromGitHub {
      owner = "WordPress";
      repo = "WordPress";
      rev = "4.9.7";
      sha256 = "1kxwk7mqhi9n334i6yb9iyi9vbl07agbbv5fzfhrcx5d2v13h48r";
    };

    basicAuthPlugin = pkgs.stdenv.mkDerivation {
      name = "basic-auth-plugin";
      # Download the theme from the wordpress site
      src = pkgs.fetchurl {
        url = https://github.com/WP-API/Basic-Auth/archive/9e9d5267c7805c024f141d115b224cdee5a10008.zip;
        sha256 = "b7f4fe0e6064040eeec2d34c27296cc69c92ed015c8d4164cf86af002fde2ddd";
      };
      # We need unzip to build this package
      buildInputs = [ pkgs.unzip ];
      # Installing simply means copying all files to the output directory
      installPhase = "mkdir -p $out; cp -R * $out/";
    };

    twentySeventeen = pkgs.stdenv.mkDerivation {
      name = "theme-twenty-seventeen";
      # Download the theme from the wordpress site
      src = pkgs.fetchurl {
        url = https://downloads.wordpress.org/theme/twentyseventeen.1.6.zip;
        sha256 = "0cch9bvap4r0775f055mynbf0d6k8zrqyn2mdwkbn6rr12hn526b";
      };
      # We need unzip to build this package
      buildInputs = [ pkgs.unzip ];
      # Installing simply means copying all files to the output directory
      installPhase = "mkdir -p $out; cp -R * $out/";
    };
  in
  {
    services.mysql = {
      enable = true;
      package = pkgs.mysql;
      initialScript = ./init.sql;
    };

    services.httpd = {
      enable = true;
      logPerVirtualHost = true;
      adminAddr="andrew@qfpl.io";
      extraModules = [
        { name = "php7"; path = "${pkgs.php}/modules/libphp7.so"; }
      ];

      virtualHosts = [
        {
          hostName = "wordpress";
          extraSubservices =
            [
              {
                serviceType = "wordpress";
                dbPassword = "wordpress";
                wordpressUploads = "/data/uploads";
                languages = [ "en_GB" ];
                package = wpPackage;
                plugins = [ basicAuthPlugin ];
                themes = [ twentySeventeen ];
              }
            ];
        }
      ];
    };

    # HTTP, HTTPS, MySQL
    networking.firewall.allowedTCPPorts = [ 80 443 3306 ];
  };
}
```

Now that I've specified the WordPress environment, I need to describe how to deploy it. For my local
tests, I'm using VirtualBox, which is supported out of the box by NixOps, such that I don't need to
specify much.

```nix
{
  wordpress =
    { config, pkgs, ... }:
    { deployment.targetEnv = "virtualbox";
      deployment.virtualbox.memorySize = 4096; # megabytes
      deployment.virtualbox.vcpu = 4; # number of cpus
      deployment.virtualbox.headless = true;
    };
}
```

With my specifications in place, I can use a couple of commands to deploy the test environment.

```
$ nixops create wp.nix wp-vbox.nix -d wp
$ nixops deploy -d wp 
```

I now have a test environment running WordPress.

