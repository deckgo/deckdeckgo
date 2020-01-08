{ lib
, terraform_0_12
, sources
, buildGoPackage
, buildGoModule
}:
let
  versionCleaner = ver: lib.removePrefix "v" ver;
in
(
  terraform_0_12.overrideAttrs (
    _oldAttrs: {
      name = "terraform-${versionCleaner sources.terraform.rev}";
      src = sources.terraform;
    }
  )
).withPlugins (
  _:
  # XXX: We repackage some providers because we need the AWS provider v2.37 for
  # RDS CA:
  # https://github.com/terraform-providers/terraform-provider-aws/issues/10417
  # We also repackage terraform to get the latest and shiniest.
    let
      buildGoPackageFromGitHub = obj:
      # TODO: replace the deprecated buildGoPackage with buildGoModule
        buildGoPackage (
          rec
          {
            version = versionCleaner obj.rev;
            name = "${obj.repo}-${version}";
            goPackagePath =
              "github.com/${obj.owner}/${obj.repo}";
            subPackages = [ "." ];
            src = obj;
            postBuild = "mv go/bin/${obj.repo}{,_v${version}}";
          }
        );
    in
      map buildGoPackageFromGitHub (
        with sources; [
          terraform-provider-aws
          terraform-provider-external
        ]
      )
)
