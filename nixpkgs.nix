{
  allowUnfree = true;
  allowBroken = true;
  permittedInsecurePackages = [
    "nodejs-16.20.2"
    "nodejs-14.21.3"    # enso-security/connectors
    "openssl-1.1.1u"    # enso-security/connectors
    "openssl-1.1.1w"    # enso-security/shipper
    "teleport-15.4.33"  # snyk
  ];
}
