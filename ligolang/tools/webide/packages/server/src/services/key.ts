import fetch from 'node-fetch';

const AUTHORIZATION_HEADER = 'Bearer ligo-ide';

export async function fetchRandomPrivateKey(network: string): Promise<string> {
  let URL = 'https://api.tez.ie/keys/florencenet/';
  if (network === 'granadanet') {
    URL = 'https://api.tez.ie/keys/granadanet/';
  }
  const response = await fetch(URL, {
    method: 'POST',
    headers: { Authorization: AUTHORIZATION_HEADER },
  });
  return response.text();
}
