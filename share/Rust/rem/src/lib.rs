use serde::Deserialize;
use dirs::home_dir;
use openssl::ssl::{SslConnector,SslMethod,SslVerifyMode,SslFiletype};
use postgres_openssl::MakeTlsConnector;
use postgres::Client;

use amkhlv::{get_psql_conf,PSQL};

#[derive(Deserialize)]
pub struct DB {
    dbnick: String
}
pub fn get_db_conf() -> Result<PSQL, Box<dyn std::error::Error>>  {
    let whichdb: DB = serde_dhall::from_file(home_dir().unwrap().join(".config").join("amkhlv").join("rem").join("config.dhall")).parse().unwrap();
    let psql_conf = get_psql_conf()?;
    let psql: &PSQL = psql_conf.get(&whichdb.dbnick).unwrap();
    Ok(psql.clone())
}

pub fn get_db_connector() -> Result<MakeTlsConnector, Box<dyn std::error::Error>>  {
    let psql = get_db_conf()?;
    let mut builder = SslConnector::builder(SslMethod::tls()).expect("unable to create sslconnector builder");
    //let x509 = openssl::x509::X509::from_pem(&std::fs::read(Path::new(&psql.sslrootcert)).unwrap()).unwrap();
    //builder.add_client_ca(&x509).expect("unable to add root cert");
    builder.set_certificate_file(&psql.sslcert,SslFiletype::PEM).expect("unable to load certificate");
    builder.set_private_key_file(&psql.sslkey,SslFiletype::PEM).expect("unable to set key");
    builder.set_ca_file(&psql.sslrootcert).expect("unable to set Root certificate");
    builder.set_verify(SslVerifyMode::PEER);
    let sslconn = builder.build();
    let connector = MakeTlsConnector::new(sslconn);
    Ok(connector)
}

pub fn get_client() -> Result<Client, Box<dyn std::error::Error>> {
    let connector = get_db_connector()?;
    let psql = get_db_conf()?;
    let conn = Client::connect(
        &format!("host={} port={} user={} sslmode=require", &psql.host, &psql.port, &psql.user), 
        connector
        ).expect("failed to create tls postgres connection");
    Ok(conn)

}