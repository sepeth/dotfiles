function FindProxyForURL(url, host) {
  var fallback = 'DIRECT';

  var proxies = {
    'yildizlib': 'PROXY libpxy.cc.yildiz.edu.tr:81; ' + fallback
  };

  var hosts = {
    'ieeexplore.ieee.org': 'yildizlib',
    '*.jstor.org': 'yildizlib',
    '*.safaribooksonline.com': 'yildizlib',
    '*.acm.org': 'yildizlib'
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
