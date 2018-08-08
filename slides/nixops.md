# Deploying with NixOps

## Nix

- Purely functional package manager.
- Specify packages as referentially transparent expressions.
- Content-based hashing of all artifacts.

::: notes
- All packages kept in a read only store
- If any build inputs change, the artifact changes
- Content-based hashing results in reliable caching and sharing of artifacts
- Each artifact can build and link against exact versions without impacting anything else
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

## Deploying WordPress

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
    url = https://downloads.wordpress.org/theme/twentyseventeen.1.6.zip;
    sha256 = "0cch9bvap4r0775f055mynbf0d6k8zrqyn2mdwkbn6rr12hn526b";
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

```nix
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

