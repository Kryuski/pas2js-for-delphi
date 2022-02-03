CREATE TABLE country
(
  iso character(2) NOT NULL,
  name character varying(80) NOT NULL,
  nicename character varying(80) NOT NULL,
  iso3 character(3) DEFAULT NULL::bpchar,
  numcode smallint,
  phonecode smallint NOT NULL,
  CONSTRAINT country_pkey PRIMARY KEY (iso)
):
