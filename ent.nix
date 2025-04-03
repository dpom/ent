{ lib,  trivialBuild, seq }:
let
  name = "ent";
  version = "2.0.0";
in
trivialBuild rec {
  pname = "${name}";
  inherit version;
  src = ./.;

  # elisp dependencies
  propagatedUserEnvPkgs = [
    seq
  ];
  buildInputs = propagatedUserEnvPkgs;
  
  meta = with lib; {
    description = "A build tool like ant but working inside emacs and using elisp syntax";
    homepage = "https://github.com/dpom/ent";
    license = licenses.gpl3;
    platforms = platforms.all;
  };
}
