
CREATE TYPE address as ( name TEXT, email TEXT );

CREATE TYPE grp AS ( name TEXT, addresses address[] );

CREATE TYPE towhom AS ( addresses address[], groups grp[] );

CREATE TABLE IF NOT EXISTS settings (
  key varchar(64) UNIQUE,
  value text
);

CREATE TABLE IF NOT EXISTS maildirs (
  name text,
  path text
);

CREATE TABLE IF NOT EXISTS mail (
  maildir varchar(128), 
  tw towhom, 
  cc towhom, 
  bcc towhom, 
  fw address, 
  s text, 
  i varchar(512), 
  d date, 
  ctypes text[], 
  p text
);

CREATE EXTENSION IF NOT EXISTS plpython3u;

CREATE OR REPLACE  FUNCTION twmatch (tw towhom, x text)
  RETURNS boolean
AS $$
  y = x.lower()
  for a in tw["addresses"]:
    if y in a["name"].lower(): 
      return True 
    if y in a["email"].lower():
      return True 
  for g in tw["groups"]:
    for a in g["addresses"]:
      if y in a["name"].lower():
        return True
      if y in a["email"].lower():
        return True 
  return False 
$$ LANGUAGE plpython3u;

CREATE OR REPLACE  FUNCTION fwmatch (fw address, x text)
  RETURNS boolean
AS $$
  y = x.lower()
  if y in fw["name"].lower(): 
    return True 
  if y in fw["email"].lower():
    return True 
  return False 
$$ LANGUAGE plpython3u;

CREATE OR REPLACE  FUNCTION mimematch (ctypes text[], x text) 
  RETURNS boolean
AS $$
  y = x.lower()
  for m in ctypes: 
    if y in m.lower(): 
      return True 
  return False 
$$ LANGUAGE plpython3u;

CREATE OR REPLACE  FUNCTION subjmatch (s text, x text) 
  RETURNS boolean
AS $$
  y = x.lower()
  if s and y in s.lower():
    return True
  else:
    return False
$$ LANGUAGE plpython3u;

