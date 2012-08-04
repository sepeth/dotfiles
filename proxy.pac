function FindProxyForURL(url, host) {
  var fallback = 'DIRECT';

  var proxies = {
    'yildiz-ktp': 'PROXY libpxy.cc.yildiz.edu.tr:81; ' + fallback
  };

  var hosts = {
    'ieeexplore.ieee.org': 'yildiz-ktp',
    '*.jstor.org': 'yildiz-ktp',
    '*.safaribooksonline.com': 'yildiz-ktp'
  };

  if (isPlainHostName(host)) {
    return 'DIRECT';
  }

  for (var h in hosts) {
    if (hosts.hasOwnProperty(h)) {
      if (shExpMatch(host, h)) {
        return proxies[hosts[h]];
      }
    }
  }

  return fallback;
}
