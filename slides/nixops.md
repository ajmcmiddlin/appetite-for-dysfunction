# Deploying with NixOps

## Nix

- Purely functional language for package management.
- Specify packages as referentially transparent expressions.
- Huge library of packages (`nixpkgs`)
- Content-based hashing of all artifacts.

::: notes
- If any build inputs change, the artifact changes
- Content-based hashing results in reliable caching and sharing of artifacts
:::

## NixOS

The purely functional Linux distribution built on top of the Nix package manager.

::: notes
- Your OS is a referentially transparent expression
- Each new build has a unique identifier
- Immediate rollback to previous states by switching some sym links
:::

## NixOps

A tool for deploying NixOS machines.

::: notes
- An OS specified with a declarative, referentially transparent expression is really useful.
- Also a great way to deploy environments
:::

## WordPress specification

##

```nix
wpPackage = pkgs.fetchFromGitHub {




};
```

##

```nix
wpPackage = pkgs.fetchFromGitHub {
  owner = "WordPress";
  repo = "WordPress";
  rev = "4.9.7";
  sha256 = "1kxwk7mqhi9n...";
};
```

##

```nix
basicAuthPlugin = pkgs.stdenv.mkDerivation {








};
```

##

```nix
basicAuthPlugin = pkgs.stdenv.mkDerivation {
  name = "basic-auth-plugin";







};
```

##

```nix
basicAuthPlugin = pkgs.stdenv.mkDerivation {
  name = "basic-auth-plugin";
  src = pkgs.fetchurl {
    url = https://github.com/WP-API/Basic-Auth/...8.zip;
    sha256 = "b7f4fe0e6064040...";
  };



};
```

##

```nix
basicAuthPlugin = pkgs.stdenv.mkDerivation {
  name = "basic-auth-plugin";
  src = pkgs.fetchurl {
    url = https://github.com/WP-API/Basic-Auth/...8.zip;
    sha256 = "b7f4fe0e6064040...";
  };

  buildInputs = [ pkgs.unzip ];
  installPhase = "mkdir -p $out; cp -R * $out/";
};
```


##


```nix
twentySeventeen = pkgs.stdenv.mkDerivation {
  name = "theme-twenty-seventeen";
  src = pkgs.fetchurl {
    url = https://.../twentyseventeen.1.6.zip;
    sha256 = "0cch9bvap4r0775f...";
  };

  buildInputs = [ pkgs.unzip ];
  installPhase = "mkdir -p $out; cp -R * $out/";
};
```

##

```nix
services.mysql = {
  enable = true;
  package = pkgs.mysql;
  initialScript = ./init.sql;
};
```

##

```nix
services.httpd = {










};
```

##

```nix
services.httpd = {
  ...
  virtualHosts = [{
    hostName = "wordpress";






  }];
};
```

##

```nix
services.httpd = {
  ...
  virtualHosts = [{
    hostName = "wordpress";
    extraSubservices = [{
      ...
      package = wpPackage;
      plugins = [ basicAuthPlugin ];
      themes = [ twentySeventeen ];
    }];
  }];
};
```

##

```nix
# HTTP, HTTPS, MySQL
networking.firewall.allowedTCPPorts = [ 80 443 3306 ];
```

## Machine specification

##

```nix
{
  wordpress =
    { config, pkgs, ... }:





}
```

##

```nix
{
  wordpress =
    { config, pkgs, ... }:
    { deployment.targetEnv = "virtualbox";



    };
}
```

##

```nix
{
  wordpress =
    { config, pkgs, ... }:
    { deployment.targetEnv = "virtualbox";
      deployment.virtualbox.memorySize = 4096; # megabytes
      deployment.virtualbox.vcpu = 4; # number of cpus

    };
}
```

##

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

## Deploying

##

```nix
$ nixops create ./wp.nix ./wp-vbox.nix -d wp


 
```

##

```nix
$ nixops create ./wp.nix ./wp-vbox.nix -d wp
$ nixops deploy -d wp

 
```

##

```nix
$ nixops create ./wp.nix ./wp-vbox.nix -d wp
$ nixops deploy -d wp
$ nixops info -d wp
 
```

##

```nix
$ nixops create ./wp.nix ./wp-vbox.nix -d wp
$ nixops deploy -d wp
$ nixops info -d wp
$ nixops ssh -d wp wordpress
```

