










const isMobile = {
  Android: () => navigator.userAgent.match(/Android/i),
  iOS: () => navigator.userAgent.match(/iPhone|iPad|iPod/i),
  any: () => navigator.userAgent.match(/Android|iPhone|iPad|iPod/i)
};

function escapeHtml(text) {
  return text
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;")
    .replace(/"/g, "&quot;")
    .replace(/'/g, "&#039;")
    .replace(/\n/g, "<br>");
}

function escapeAttr(text) {
  return text
    .replace(/&/g, "&amp;")
    .replace(/"/g, "&quot;")
    .replace(/'/g, "&#039;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;");
}

const SAFE_URI_SCHEME = /^(https?:|simplex:|mailto:|tel:)/i;

function safeHref(uri) {
  if (SAFE_URI_SCHEME.test(uri)) return escapeAttr(uri);
  return escapeAttr(`javascript:void(alert('Potentially malicious link blocked:\\n'+${JSON.stringify(uri)}))`);
}

function getSimplexLinkDescr(linkType) {
  switch (linkType) {
    case 'contact': return 'SimpleX contact address';
    case 'invitation': return 'SimpleX one-time invitation';
    case 'group': return 'SimpleX group link';
    case 'channel': return 'SimpleX channel link';
    case 'relay': return 'SimpleX relay link';
    default: return 'SimpleX link';
  }
}

function viaHost(smpHosts) {
  const first = smpHosts[0] ?? '?';
  return `via ${first}`;
}

function isCurrentSite(uri) {
  return uri.startsWith("https://simplex.chat") || uri.startsWith("https://www.simplex.chat")
}

function targetBlank(uri) {
  return isCurrentSite(uri) ? '' : ' target="_blank"'
}

function renderMarkdown(fts) {
  let html = '';
  for (const ft of fts) {
    const { format, text } = ft;
    if (!format) {
      html += escapeHtml(text);
      continue;
    }
    try {
      switch (format.type) {
        case 'bold':
          html += `<strong>${escapeHtml(text)}</strong>`;
          break;
        case 'italic':
          html += `<em>${escapeHtml(text)}</em>`;
          break;
        case 'strikeThrough':
          html += `<s>${escapeHtml(text)}</s>`;
          break;
        case 'snippet':
          html += `<span style="font-family: monospace;">${escapeHtml(text)}</span>`;
          break;
        case 'secret':
          html += `<span class="secret">${escapeHtml(text)}</span>`;
          break;
        case 'small':
          html += `<span class="small-text">${escapeHtml(text)}</span>`;
          break;
        case 'colored':
          html += `<span class="${(format.color || '').replace(/[^a-zA-Z0-9_-]/g, '')}">${escapeHtml(text)}</span>`;
          break;
        case 'uri': {
          let href = text.startsWith('http://') || text.startsWith('https://') || text.startsWith('simplex:/') ? text : 'https://' + text;
          html += `<a href="${safeHref(href)}"${targetBlank(href)}>${escapeHtml(text)}</a>`;
          break;
        }
        case 'hyperLink': {
          const { showText, linkUri } = format;
          html += `<a href="${safeHref(linkUri)}"${targetBlank(linkUri)}>${escapeHtml(showText ?? linkUri)}</a>`;
          break;
        }
        case 'simplexLink': {
          const { showText, linkType, simplexUri, smpHosts } = format;
          const linkText = showText ? escapeHtml(showText) : getSimplexLinkDescr(linkType);
          html += `<a href="${safeHref(platformSimplexUri(simplexUri))}" target="_blank">${linkText} <em>(${escapeHtml(viaHost(smpHosts))})</em></a>`;
          break;
        }
        case 'command':
          html += `<span style="font-family: monospace;">${escapeHtml(text)}</span>`;
          break;
        case 'mention':
          html += `<strong>${escapeHtml(text)}</strong>`;
          break;
        case 'email':
          html += `<a href="mailto:${escapeAttr(text)}">${escapeHtml(text)}</a>`;
          break;
        case 'phone':
          html += `<a href="tel:${escapeAttr(text)}">${escapeHtml(text)}</a>`;
          break;
        case 'unknown':
          html += escapeHtml(text);
          break;
        default:
          html += escapeHtml(text);
      }
    } catch(e) {
      console.log(e);
      html += escapeHtml(text);
    }
  }
  return html;
}

const simplexAddressRegexp = /^simplex:\/([a-z]+)#(.+)/i;

const simplexShortLinkTypes = ["a", "c", "g", "i", "r"];

function platformSimplexUri(uri) {
  if (isMobile.any()) return uri;
  const res = uri.match(simplexAddressRegexp);
  if (!res || !Array.isArray(res) || res.length < 3) return uri;
  const linkType = res[1];
  const fragment = res[2];
  if (simplexShortLinkTypes.includes(linkType)) {
    const queryIndex = fragment.indexOf('?');
    if (queryIndex === -1) return uri;
    const hashPart = fragment.substring(0, queryIndex);
    const queryStr = fragment.substring(queryIndex + 1);
    const params = new URLSearchParams(queryStr);
    const host = params.get('h');
    if (!host) return uri;
    params.delete('h');
    let newFragment = hashPart;
    const remainingParams = params.toString();
    if (remainingParams) newFragment += '?' + remainingParams;
    return `https://${host}:/${linkType}#${newFragment}`;
  } else {
    return `https://simplex.chat/${linkType}#${fragment}`;
  }
}

(function() {

var QRCode=function(t){"use strict";var r,e=function(){return"function"==typeof Promise&&Promise.prototype&&Promise.prototype.then},n=[0,26,44,70,100,134,172,196,242,292,346,404,466,532,581,655,733,815,901,991,1085,1156,1258,1364,1474,1588,1706,1828,1921,2051,2185,2323,2465,2611,2761,2876,3034,3196,3362,3532,3706],o=function(t){if(!t)throw new Error('"version" cannot be null or undefined');if(t<1||t>40)throw new Error('"version" should be in range from 1 to 40');return 4*t+17},a=function(t){return n[t]},i=function(t){for(var r=0;0!==t;)r++,t>>>=1;return r},u=function(t){if("function"!=typeof t)throw new Error('"toSJISFunc" is not a valid function.');r=t},s=function(){return void 0!==r},f=function(t){return r(t)};function h(t,r){return t(r={exports:{}},r.exports),r.exports}var c=h((function(t,r){r.L={bit:1},r.M={bit:0},r.Q={bit:3},r.H={bit:2},r.isValid=function(t){return t&&void 0!==t.bit&&t.bit>=0&&t.bit<4},r.from=function(t,e){if(r.isValid(t))return t;try{return function(t){if("string"!=typeof t)throw new Error("Param is not a string");switch(t.toLowerCase()){case"l":case"low":return r.L;case"m":case"medium":return r.M;case"q":case"quartile":return r.Q;case"h":case"high":return r.H;default:throw new Error("Unknown EC Level: "+t)}}(t)}catch(t){return e}}}));function g(){this.buffer=[],this.length=0}c.L,c.M,c.Q,c.H,c.isValid,g.prototype={get:function(t){var r=Math.floor(t/8);return 1==(this.buffer[r]>>>7-t%8&1)},put:function(t,r){for(var e=0;e<r;e++)this.putBit(1==(t>>>r-e-1&1))},getLengthInBits:function(){return this.length},putBit:function(t){var r=Math.floor(this.length/8);this.buffer.length<=r&&this.buffer.push(0),t&&(this.buffer[r]|=128>>>this.length%8),this.length++}};var d=g;function l(t){if(!t||t<1)throw new Error("BitMatrix size must be defined and greater than 0");this.size=t,this.data=new Uint8Array(t*t),this.reservedBit=new Uint8Array(t*t)}l.prototype.set=function(t,r,e,n){var o=t*this.size+r;this.data[o]=e,n&&(this.reservedBit[o]=!0)},l.prototype.get=function(t,r){return this.data[t*this.size+r]},l.prototype.xor=function(t,r,e){this.data[t*this.size+r]^=e},l.prototype.isReserved=function(t,r){return this.reservedBit[t*this.size+r]};var v=l,p=h((function(t,r){var e=o;r.getRowColCoords=function(t){if(1===t)return[];for(var r=Math.floor(t/7)+2,n=e(t),o=145===n?26:2*Math.ceil((n-13)/(2*r-2)),a=[n-7],i=1;i<r-1;i++)a[i]=a[i-1]-o;return a.push(6),a.reverse()},r.getPositions=function(t){for(var e=[],n=r.getRowColCoords(t),o=n.length,a=0;a<o;a++)for(var i=0;i<o;i++)0===a&&0===i||0===a&&i===o-1||a===o-1&&0===i||e.push([n[a],n[i]]);return e}}));p.getRowColCoords,p.getPositions;var w=o,m=function(t){var r=w(t);return[[0,0],[r-7,0],[0,r-7]]},E=h((function(t,r){r.Patterns={PATTERN000:0,PATTERN001:1,PATTERN010:2,PATTERN011:3,PATTERN100:4,PATTERN101:5,PATTERN110:6,PATTERN111:7};var e=3,n=3,o=40,a=10;function i(t,e,n){switch(t){case r.Patterns.PATTERN000:return(e+n)%2==0;case r.Patterns.PATTERN001:return e%2==0;case r.Patterns.PATTERN010:return n%3==0;case r.Patterns.PATTERN011:return(e+n)%3==0;case r.Patterns.PATTERN100:return(Math.floor(e/2)+Math.floor(n/3))%2==0;case r.Patterns.PATTERN101:return e*n%2+e*n%3==0;case r.Patterns.PATTERN110:return(e*n%2+e*n%3)%2==0;case r.Patterns.PATTERN111:return(e*n%3+(e+n)%2)%2==0;default:throw new Error("bad maskPattern:"+t)}}r.isValid=function(t){return null!=t&&""!==t&&!isNaN(t)&&t>=0&&t<=7},r.from=function(t){return r.isValid(t)?parseInt(t,10):void 0},r.getPenaltyN1=function(t){for(var r=t.size,n=0,o=0,a=0,i=null,u=null,s=0;s<r;s++){o=a=0,i=u=null;for(var f=0;f<r;f++){var h=t.get(s,f);h===i?o++:(o>=5&&(n+=e+(o-5)),i=h,o=1),(h=t.get(f,s))===u?a++:(a>=5&&(n+=e+(a-5)),u=h,a=1)}o>=5&&(n+=e+(o-5)),a>=5&&(n+=e+(a-5))}return n},r.getPenaltyN2=function(t){for(var r=t.size,e=0,o=0;o<r-1;o++)for(var a=0;a<r-1;a++){var i=t.get(o,a)+t.get(o,a+1)+t.get(o+1,a)+t.get(o+1,a+1);4!==i&&0!==i||e++}return e*n},r.getPenaltyN3=function(t){for(var r=t.size,e=0,n=0,a=0,i=0;i<r;i++){n=a=0;for(var u=0;u<r;u++)n=n<<1&2047|t.get(i,u),u>=10&&(1488===n||93===n)&&e++,a=a<<1&2047|t.get(u,i),u>=10&&(1488===a||93===a)&&e++}return e*o},r.getPenaltyN4=function(t){for(var r=0,e=t.data.length,n=0;n<e;n++)r+=t.data[n];return Math.abs(Math.ceil(100*r/e/5)-10)*a},r.applyMask=function(t,r){for(var e=r.size,n=0;n<e;n++)for(var o=0;o<e;o++)r.isReserved(o,n)||r.xor(o,n,i(t,o,n))},r.getBestMask=function(t,e){for(var n=Object.keys(r.Patterns).length,o=0,a=1/0,i=0;i<n;i++){e(i),r.applyMask(i,t);var u=r.getPenaltyN1(t)+r.getPenaltyN2(t)+r.getPenaltyN3(t)+r.getPenaltyN4(t);r.applyMask(i,t),u<a&&(a=u,o=i)}return o}}));E.Patterns,E.isValid,E.getPenaltyN1,E.getPenaltyN2,E.getPenaltyN3,E.getPenaltyN4,E.applyMask,E.getBestMask;var y=[1,1,1,1,1,1,1,1,1,1,2,2,1,2,2,4,1,2,4,4,2,4,4,4,2,4,6,5,2,4,6,6,2,5,8,8,4,5,8,8,4,5,8,11,4,8,10,11,4,9,12,16,4,9,16,16,6,10,12,18,6,10,17,16,6,11,16,19,6,13,18,21,7,14,21,25,8,16,20,25,8,17,23,25,9,17,23,34,9,18,25,30,10,20,27,32,12,21,29,35,12,23,34,37,12,25,34,40,13,26,35,42,14,28,38,45,15,29,40,48,16,31,43,51,17,33,45,54,18,35,48,57,19,37,51,60,19,38,53,63,20,40,56,66,21,43,59,70,22,45,62,74,24,47,65,77,25,49,68,81],A=[7,10,13,17,10,16,22,28,15,26,36,44,20,36,52,64,26,48,72,88,36,64,96,112,40,72,108,130,48,88,132,156,60,110,160,192,72,130,192,224,80,150,224,264,96,176,260,308,104,198,288,352,120,216,320,384,132,240,360,432,144,280,408,480,168,308,448,532,180,338,504,588,196,364,546,650,224,416,600,700,224,442,644,750,252,476,690,816,270,504,750,900,300,560,810,960,312,588,870,1050,336,644,952,1110,360,700,1020,1200,390,728,1050,1260,420,784,1140,1350,450,812,1200,1440,480,868,1290,1530,510,924,1350,1620,540,980,1440,1710,570,1036,1530,1800,570,1064,1590,1890,600,1120,1680,1980,630,1204,1770,2100,660,1260,1860,2220,720,1316,1950,2310,750,1372,2040,2430],I=function(t,r){switch(r){case c.L:return y[4*(t-1)+0];case c.M:return y[4*(t-1)+1];case c.Q:return y[4*(t-1)+2];case c.H:return y[4*(t-1)+3];default:return}},M=function(t,r){switch(r){case c.L:return A[4*(t-1)+0];case c.M:return A[4*(t-1)+1];case c.Q:return A[4*(t-1)+2];case c.H:return A[4*(t-1)+3];default:return}},N=new Uint8Array(512),B=new Uint8Array(256);!function(){for(var t=1,r=0;r<255;r++)N[r]=t,B[t]=r,256&(t<<=1)&&(t^=285);for(var e=255;e<512;e++)N[e]=N[e-255]}();var C=function(t){return N[t]},P=function(t,r){return 0===t||0===r?0:N[B[t]+B[r]]},R=h((function(t,r){r.mul=function(t,r){for(var e=new Uint8Array(t.length+r.length-1),n=0;n<t.length;n++)for(var o=0;o<r.length;o++)e[n+o]^=P(t[n],r[o]);return e},r.mod=function(t,r){for(var e=new Uint8Array(t);e.length-r.length>=0;){for(var n=e[0],o=0;o<r.length;o++)e[o]^=P(r[o],n);for(var a=0;a<e.length&&0===e[a];)a++;e=e.slice(a)}return e},r.generateECPolynomial=function(t){for(var e=new Uint8Array([1]),n=0;n<t;n++)e=r.mul(e,new Uint8Array([1,C(n)]));return e}}));function T(t){this.genPoly=void 0,this.degree=t,this.degree&&this.initialize(this.degree)}R.mul,R.mod,R.generateECPolynomial,T.prototype.initialize=function(t){this.degree=t,this.genPoly=R.generateECPolynomial(this.degree)},T.prototype.encode=function(t){if(!this.genPoly)throw new Error("Encoder not initialized");var r=new Uint8Array(t.length+this.degree);r.set(t);var e=R.mod(r,this.genPoly),n=this.degree-e.length;if(n>0){var o=new Uint8Array(this.degree);return o.set(e,n),o}return e};var L=T,b=function(t){return!isNaN(t)&&t>=1&&t<=40},U="(?:[u3000-u303F]|[u3040-u309F]|[u30A0-u30FF]|[uFF00-uFFEF]|[u4E00-u9FAF]|[u2605-u2606]|[u2190-u2195]|u203B|[u2010u2015u2018u2019u2025u2026u201Cu201Du2225u2260]|[u0391-u0451]|[u00A7u00A8u00B1u00B4u00D7u00F7])+",x="(?:(?![A-Z0-9 $%*+\\-./:]|"+(U=U.replace(/u/g,"\\u"))+")(?:.|[\r\n]))+",k=new RegExp(U,"g"),F=new RegExp("[^A-Z0-9 $%*+\\-./:]+","g"),S=new RegExp(x,"g"),D=new RegExp("[0-9]+","g"),Y=new RegExp("[A-Z $%*+\\-./:]+","g"),_=new RegExp("^"+U+"$"),z=new RegExp("^[0-9]+$"),H=new RegExp("^[A-Z0-9 $%*+\\-./:]+$"),J={KANJI:k,BYTE_KANJI:F,BYTE:S,NUMERIC:D,ALPHANUMERIC:Y,testKanji:function(t){return _.test(t)},testNumeric:function(t){return z.test(t)},testAlphanumeric:function(t){return H.test(t)}},K=h((function(t,r){r.NUMERIC={id:"Numeric",bit:1,ccBits:[10,12,14]},r.ALPHANUMERIC={id:"Alphanumeric",bit:2,ccBits:[9,11,13]},r.BYTE={id:"Byte",bit:4,ccBits:[8,16,16]},r.KANJI={id:"Kanji",bit:8,ccBits:[8,10,12]},r.MIXED={bit:-1},r.getCharCountIndicator=function(t,r){if(!t.ccBits)throw new Error("Invalid mode: "+t);if(!b(r))throw new Error("Invalid version: "+r);return r>=1&&r<10?t.ccBits[0]:r<27?t.ccBits[1]:t.ccBits[2]},r.getBestModeForData=function(t){return J.testNumeric(t)?r.NUMERIC:J.testAlphanumeric(t)?r.ALPHANUMERIC:J.testKanji(t)?r.KANJI:r.BYTE},r.toString=function(t){if(t&&t.id)return t.id;throw new Error("Invalid mode")},r.isValid=function(t){return t&&t.bit&&t.ccBits},r.from=function(t,e){if(r.isValid(t))return t;try{return function(t){if("string"!=typeof t)throw new Error("Param is not a string");switch(t.toLowerCase()){case"numeric":return r.NUMERIC;case"alphanumeric":return r.ALPHANUMERIC;case"kanji":return r.KANJI;case"byte":return r.BYTE;default:throw new Error("Unknown mode: "+t)}}(t)}catch(t){return e}}}));K.NUMERIC,K.ALPHANUMERIC,K.BYTE,K.KANJI,K.MIXED,K.getCharCountIndicator,K.getBestModeForData,K.isValid;var O=h((function(t,r){var e=i(7973);function n(t,r){return K.getCharCountIndicator(t,r)+4}function o(t,r){var e=0;return t.forEach((function(t){var o=n(t.mode,r);e+=o+t.getBitsLength()})),e}r.from=function(t,r){return b(t)?parseInt(t,10):r},r.getCapacity=function(t,r,e){if(!b(t))throw new Error("Invalid QR Code version");void 0===e&&(e=K.BYTE);var o=8*(a(t)-M(t,r));if(e===K.MIXED)return o;var i=o-n(e,t);switch(e){case K.NUMERIC:return Math.floor(i/10*3);case K.ALPHANUMERIC:return Math.floor(i/11*2);case K.KANJI:return Math.floor(i/13);case K.BYTE:default:return Math.floor(i/8)}},r.getBestVersionForData=function(t,e){var n,a=c.from(e,c.M);if(Array.isArray(t)){if(t.length>1)return function(t,e){for(var n=1;n<=40;n++){if(o(t,n)<=r.getCapacity(n,e,K.MIXED))return n}}(t,a);if(0===t.length)return 1;n=t[0]}else n=t;return function(t,e,n){for(var o=1;o<=40;o++)if(e<=r.getCapacity(o,n,t))return o}(n.mode,n.getLength(),a)},r.getEncodedBits=function(t){if(!b(t)||t<7)throw new Error("Invalid QR Code version");for(var r=t<<12;i(r)-e>=0;)r^=7973<<i(r)-e;return t<<12|r}}));O.getCapacity,O.getBestVersionForData,O.getEncodedBits;var Q=i(1335),V=function(t,r){for(var e=t.bit<<3|r,n=e<<10;i(n)-Q>=0;)n^=1335<<i(n)-Q;return 21522^(e<<10|n)};function q(t){this.mode=K.NUMERIC,this.data=t.toString()}q.getBitsLength=function(t){return 10*Math.floor(t/3)+(t%3?t%3*3+1:0)},q.prototype.getLength=function(){return this.data.length},q.prototype.getBitsLength=function(){return q.getBitsLength(this.data.length)},q.prototype.write=function(t){var r,e,n;for(r=0;r+3<=this.data.length;r+=3)e=this.data.substr(r,3),n=parseInt(e,10),t.put(n,10);var o=this.data.length-r;o>0&&(e=this.data.substr(r),n=parseInt(e,10),t.put(n,3*o+1))};var j=q,$=["0","1","2","3","4","5","6","7","8","9","A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z"," ","$","%","*","+","-",".","/",":"];function X(t){this.mode=K.ALPHANUMERIC,this.data=t}X.getBitsLength=function(t){return 11*Math.floor(t/2)+t%2*6},X.prototype.getLength=function(){return this.data.length},X.prototype.getBitsLength=function(){return X.getBitsLength(this.data.length)},X.prototype.write=function(t){var r;for(r=0;r+2<=this.data.length;r+=2){var e=45*$.indexOf(this.data[r]);e+=$.indexOf(this.data[r+1]),t.put(e,11)}this.data.length%2&&t.put($.indexOf(this.data[r]),6)};var Z=X;function W(t){this.mode=K.BYTE,"string"==typeof t&&(t=function(t){for(var r=[],e=t.length,n=0;n<e;n++){var o=t.charCodeAt(n);if(o>=55296&&o<=56319&&e>n+1){var a=t.charCodeAt(n+1);a>=56320&&a<=57343&&(o=1024*(o-55296)+a-56320+65536,n+=1)}o<128?r.push(o):o<2048?(r.push(o>>6|192),r.push(63&o|128)):o<55296||o>=57344&&o<65536?(r.push(o>>12|224),r.push(o>>6&63|128),r.push(63&o|128)):o>=65536&&o<=1114111?(r.push(o>>18|240),r.push(o>>12&63|128),r.push(o>>6&63|128),r.push(63&o|128)):r.push(239,191,189)}return new Uint8Array(r).buffer}(t)),this.data=new Uint8Array(t)}W.getBitsLength=function(t){return 8*t},W.prototype.getLength=function(){return this.data.length},W.prototype.getBitsLength=function(){return W.getBitsLength(this.data.length)},W.prototype.write=function(t){for(var r=0,e=this.data.length;r<e;r++)t.put(this.data[r],8)};var G=W;function tt(t){this.mode=K.KANJI,this.data=t}tt.getBitsLength=function(t){return 13*t},tt.prototype.getLength=function(){return this.data.length},tt.prototype.getBitsLength=function(){return tt.getBitsLength(this.data.length)},tt.prototype.write=function(t){var r;for(r=0;r<this.data.length;r++){var e=f(this.data[r]);if(e>=33088&&e<=40956)e-=33088;else{if(!(e>=57408&&e<=60351))throw new Error("Invalid SJIS character: "+this.data[r]+"\nMake sure your charset is UTF-8");e-=49472}e=192*(e>>>8&255)+(255&e),t.put(e,13)}};var rt=tt,et=h((function(t){var r={single_source_shortest_paths:function(t,e,n){var o={},a={};a[e]=0;var i,u,s,f,h,c,g,d=r.PriorityQueue.make();for(d.push(e,0);!d.empty();)for(s in u=(i=d.pop()).value,f=i.cost,h=t[u]||{})h.hasOwnProperty(s)&&(c=f+h[s],g=a[s],(void 0===a[s]||g>c)&&(a[s]=c,d.push(s,c),o[s]=u));if(void 0!==n&&void 0===a[n]){var l=["Could not find a path from ",e," to ",n,"."].join("");throw new Error(l)}return o},extract_shortest_path_from_predecessor_list:function(t,r){for(var e=[],n=r;n;)e.push(n),n=t[n];return e.reverse(),e},find_path:function(t,e,n){var o=r.single_source_shortest_paths(t,e,n);return r.extract_shortest_path_from_predecessor_list(o,n)},PriorityQueue:{make:function(t){var e,n=r.PriorityQueue,o={};for(e in t=t||{},n)n.hasOwnProperty(e)&&(o[e]=n[e]);return o.queue=[],o.sorter=t.sorter||n.default_sorter,o},default_sorter:function(t,r){return t.cost-r.cost},push:function(t,r){var e={value:t,cost:r};this.queue.push(e),this.queue.sort(this.sorter)},pop:function(){return this.queue.shift()},empty:function(){return 0===this.queue.length}}};t.exports=r})),nt=h((function(t,r){function e(t){return unescape(encodeURIComponent(t)).length}function n(t,r,e){for(var n,o=[];null!==(n=t.exec(e));)o.push({data:n[0],index:n.index,mode:r,length:n[0].length});return o}function o(t){var r,e,o=n(J.NUMERIC,K.NUMERIC,t),a=n(J.ALPHANUMERIC,K.ALPHANUMERIC,t);return s()?(r=n(J.BYTE,K.BYTE,t),e=n(J.KANJI,K.KANJI,t)):(r=n(J.BYTE_KANJI,K.BYTE,t),e=[]),o.concat(a,r,e).sort((function(t,r){return t.index-r.index})).map((function(t){return{data:t.data,mode:t.mode,length:t.length}}))}function a(t,r){switch(r){case K.NUMERIC:return j.getBitsLength(t);case K.ALPHANUMERIC:return Z.getBitsLength(t);case K.KANJI:return rt.getBitsLength(t);case K.BYTE:return G.getBitsLength(t)}}function i(t,r){var e,n=K.getBestModeForData(t);if((e=K.from(r,n))!==K.BYTE&&e.bit<n.bit)throw new Error('"'+t+'" cannot be encoded with mode '+K.toString(e)+".\n Suggested mode is: "+K.toString(n));switch(e!==K.KANJI||s()||(e=K.BYTE),e){case K.NUMERIC:return new j(t);case K.ALPHANUMERIC:return new Z(t);case K.KANJI:return new rt(t);case K.BYTE:return new G(t)}}r.fromArray=function(t){return t.reduce((function(t,r){return"string"==typeof r?t.push(i(r,null)):r.data&&t.push(i(r.data,r.mode)),t}),[])},r.fromString=function(t,n){for(var i=function(t,r){for(var e={},n={start:{}},o=["start"],i=0;i<t.length;i++){for(var u=t[i],s=[],f=0;f<u.length;f++){var h=u[f],c=""+i+f;s.push(c),e[c]={node:h,lastCount:0},n[c]={};for(var g=0;g<o.length;g++){var d=o[g];e[d]&&e[d].node.mode===h.mode?(n[d][c]=a(e[d].lastCount+h.length,h.mode)-a(e[d].lastCount,h.mode),e[d].lastCount+=h.length):(e[d]&&(e[d].lastCount=h.length),n[d][c]=a(h.length,h.mode)+4+K.getCharCountIndicator(h.mode,r))}}o=s}for(var l=0;l<o.length;l++)n[o[l]].end=0;return{map:n,table:e}}(function(t){for(var r=[],n=0;n<t.length;n++){var o=t[n];switch(o.mode){case K.NUMERIC:r.push([o,{data:o.data,mode:K.ALPHANUMERIC,length:o.length},{data:o.data,mode:K.BYTE,length:o.length}]);break;case K.ALPHANUMERIC:r.push([o,{data:o.data,mode:K.BYTE,length:o.length}]);break;case K.KANJI:r.push([o,{data:o.data,mode:K.BYTE,length:e(o.data)}]);break;case K.BYTE:r.push([{data:o.data,mode:K.BYTE,length:e(o.data)}])}}return r}(o(t)),n),u=et.find_path(i.map,"start","end"),s=[],f=1;f<u.length-1;f++)s.push(i.table[u[f]].node);return r.fromArray(function(t){return t.reduce((function(t,r){var e=t.length-1>=0?t[t.length-1]:null;return e&&e.mode===r.mode?(t[t.length-1].data+=r.data,t):(t.push(r),t)}),[])}(s))},r.rawSplit=function(t){return r.fromArray(o(t))}}));function ot(t,r,e){var n,o,a=t.size,i=V(r,e);for(n=0;n<15;n++)o=1==(i>>n&1),n<6?t.set(n,8,o,!0):n<8?t.set(n+1,8,o,!0):t.set(a-15+n,8,o,!0),n<8?t.set(8,a-n-1,o,!0):n<9?t.set(8,15-n-1+1,o,!0):t.set(8,15-n-1,o,!0);t.set(a-8,8,1,!0)}function at(t,r,e){var n=new d;e.forEach((function(r){n.put(r.mode.bit,4),n.put(r.getLength(),K.getCharCountIndicator(r.mode,t)),r.write(n)}));var o=8*(a(t)-M(t,r));for(n.getLengthInBits()+4<=o&&n.put(0,4);n.getLengthInBits()%8!=0;)n.putBit(0);for(var i=(o-n.getLengthInBits())/8,u=0;u<i;u++)n.put(u%2?17:236,8);return function(t,r,e){for(var n=a(r),o=M(r,e),i=n-o,u=I(r,e),s=u-n%u,f=Math.floor(n/u),h=Math.floor(i/u),c=h+1,g=f-h,d=new L(g),l=0,v=new Array(u),p=new Array(u),w=0,m=new Uint8Array(t.buffer),E=0;E<u;E++){var y=E<s?h:c;v[E]=m.slice(l,l+y),p[E]=d.encode(v[E]),l+=y,w=Math.max(w,y)}var A,N,B=new Uint8Array(n),C=0;for(A=0;A<w;A++)for(N=0;N<u;N++)A<v[N].length&&(B[C++]=v[N][A]);for(A=0;A<g;A++)for(N=0;N<u;N++)B[C++]=p[N][A];return B}(n,t,r)}function it(t,r,e,n){var a;if(Array.isArray(t))a=nt.fromArray(t);else{if("string"!=typeof t)throw new Error("Invalid data");var i=r;if(!i){var u=nt.rawSplit(t);i=O.getBestVersionForData(u,e)}a=nt.fromString(t,i||40)}var s=O.getBestVersionForData(a,e);if(!s)throw new Error("The amount of data is too big to be stored in a QR Code");if(r){if(r<s)throw new Error("\nThe chosen QR Code version cannot contain this amount of data.\nMinimum version required to store current data is: "+s+".\n")}else r=s;var f=at(r,e,a),h=o(r),c=new v(h);return function(t,r){for(var e=t.size,n=m(r),o=0;o<n.length;o++)for(var a=n[o][0],i=n[o][1],u=-1;u<=7;u++)if(!(a+u<=-1||e<=a+u))for(var s=-1;s<=7;s++)i+s<=-1||e<=i+s||(u>=0&&u<=6&&(0===s||6===s)||s>=0&&s<=6&&(0===u||6===u)||u>=2&&u<=4&&s>=2&&s<=4?t.set(a+u,i+s,!0,!0):t.set(a+u,i+s,!1,!0))}(c,r),function(t){for(var r=t.size,e=8;e<r-8;e++){var n=e%2==0;t.set(e,6,n,!0),t.set(6,e,n,!0)}}(c),function(t,r){for(var e=p.getPositions(r),n=0;n<e.length;n++)for(var o=e[n][0],a=e[n][1],i=-2;i<=2;i++)for(var u=-2;u<=2;u++)-2===i||2===i||-2===u||2===u||0===i&&0===u?t.set(o+i,a+u,!0,!0):t.set(o+i,a+u,!1,!0)}(c,r),ot(c,e,0),r>=7&&function(t,r){for(var e,n,o,a=t.size,i=O.getEncodedBits(r),u=0;u<18;u++)e=Math.floor(u/3),n=u%3+a-8-3,o=1==(i>>u&1),t.set(e,n,o,!0),t.set(n,e,o,!0)}(c,r),function(t,r){for(var e=t.size,n=-1,o=e-1,a=7,i=0,u=e-1;u>0;u-=2)for(6===u&&u--;;){for(var s=0;s<2;s++)if(!t.isReserved(o,u-s)){var f=!1;i<r.length&&(f=1==(r[i]>>>a&1)),t.set(o,u-s,f),-1===--a&&(i++,a=7)}if((o+=n)<0||e<=o){o-=n,n=-n;break}}}(c,f),isNaN(n)&&(n=E.getBestMask(c,ot.bind(null,c,e))),E.applyMask(n,c),ot(c,e,n),{modules:c,version:r,errorCorrectionLevel:e,maskPattern:n,segments:a}}nt.fromArray,nt.fromString,nt.rawSplit;var ut=function(t,r){if(void 0===t||""===t)throw new Error("No input text");var e,n,o=c.M;return void 0!==r&&(o=c.from(r.errorCorrectionLevel,c.M),e=O.from(r.version),n=E.from(r.maskPattern),r.toSJISFunc&&u(r.toSJISFunc)),it(t,e,o,n)},st=h((function(t,r){function e(t){if("number"==typeof t&&(t=t.toString()),"string"!=typeof t)throw new Error("Color should be defined as hex string");var r=t.slice().replace("#","").split("");if(r.length<3||5===r.length||r.length>8)throw new Error("Invalid hex color: "+t);3!==r.length&&4!==r.length||(r=Array.prototype.concat.apply([],r.map((function(t){return[t,t]})))),6===r.length&&r.push("F","F");var e=parseInt(r.join(""),16);return{r:e>>24&255,g:e>>16&255,b:e>>8&255,a:255&e,hex:"#"+r.slice(0,6).join("")}}r.getOptions=function(t){t||(t={}),t.color||(t.color={});var r=void 0===t.margin||null===t.margin||t.margin<0?4:t.margin,n=t.width&&t.width>=21?t.width:void 0,o=t.scale||4;return{width:n,scale:n?4:o,margin:r,color:{dark:e(t.color.dark||"#000000ff"),light:e(t.color.light||"#ffffffff")},type:t.type,rendererOpts:t.rendererOpts||{}}},r.getScale=function(t,r){return r.width&&r.width>=t+2*r.margin?r.width/(t+2*r.margin):r.scale},r.getImageWidth=function(t,e){var n=r.getScale(t,e);return Math.floor((t+2*e.margin)*n)},r.qrToImageData=function(t,e,n){for(var o=e.modules.size,a=e.modules.data,i=r.getScale(o,n),u=Math.floor((o+2*n.margin)*i),s=n.margin*i,f=[n.color.light,n.color.dark],h=0;h<u;h++)for(var c=0;c<u;c++){var g=4*(h*u+c),d=n.color.light;if(h>=s&&c>=s&&h<u-s&&c<u-s)d=f[a[Math.floor((h-s)/i)*o+Math.floor((c-s)/i)]?1:0];t[g++]=d.r,t[g++]=d.g,t[g++]=d.b,t[g]=d.a}}}));st.getOptions,st.getScale,st.getImageWidth,st.qrToImageData;var ft=h((function(t,r){r.render=function(t,r,e){var n=e,o=r;void 0!==n||r&&r.getContext||(n=r,r=void 0),r||(o=function(){try{return document.createElement("canvas")}catch(t){throw new Error("You need to specify a canvas element")}}()),n=st.getOptions(n);var a=st.getImageWidth(t.modules.size,n),i=o.getContext("2d"),u=i.createImageData(a,a);return st.qrToImageData(u.data,t,n),function(t,r,e){t.clearRect(0,0,r.width,r.height),r.style||(r.style={}),r.height=e,r.width=e,r.style.height=e+"px",r.style.width=e+"px"}(i,o,a),i.putImageData(u,0,0),o},r.renderToDataURL=function(t,e,n){var o=n;void 0!==o||e&&e.getContext||(o=e,e=void 0),o||(o={});var a=r.render(t,e,o),i=o.type||"image/png",u=o.rendererOpts||{};return a.toDataURL(i,u.quality)}}));function ht(t,r){var e=t.a/255,n=r+'="'+t.hex+'"';return e<1?n+" "+r+'-opacity="'+e.toFixed(2).slice(1)+'"':n}function ct(t,r,e){var n=t+r;return void 0!==e&&(n+=" "+e),n}ft.render,ft.renderToDataURL;var gt=function(t,r,e){var n=st.getOptions(r),o=t.modules.size,a=t.modules.data,i=o+2*n.margin,u=n.color.light.a?"<path "+ht(n.color.light,"fill")+' d="M0 0h'+i+"v"+i+'H0z"/>':"",s="<path "+ht(n.color.dark,"stroke")+' d="'+function(t,r,e){for(var n="",o=0,a=!1,i=0,u=0;u<t.length;u++){var s=Math.floor(u%r),f=Math.floor(u/r);s||a||(a=!0),t[u]?(i++,u>0&&s>0&&t[u-1]||(n+=a?ct("M",s+e,.5+f+e):ct("m",o,0),o=0,a=!1),s+1<r&&t[u+1]||(n+=ct("h",i),i=0)):o++}return n}(a,o,n.margin)+'"/>',f='viewBox="0 0 '+i+" "+i+'"',h='<svg xmlns="http://www.w3.org/2000/svg" '+(n.width?'width="'+n.width+'" height="'+n.width+'" ':"")+f+' shape-rendering="crispEdges">'+u+s+"</svg>\n";return"function"==typeof e&&e(null,h),h};function dt(t,r,n,o,a){var i=[].slice.call(arguments,1),u=i.length,s="function"==typeof i[u-1];if(!s&&!e())throw new Error("Callback required as last argument");if(!s){if(u<1)throw new Error("Too few arguments provided");return 1===u?(n=r,r=o=void 0):2!==u||r.getContext||(o=n,n=r,r=void 0),new Promise((function(e,a){try{var i=ut(n,o);e(t(i,r,o))}catch(t){a(t)}}))}if(u<2)throw new Error("Too few arguments provided");2===u?(a=n,n=r,r=o=void 0):3===u&&(r.getContext&&void 0===a?(a=o,o=void 0):(a=o,o=n,n=r,r=void 0));try{var f=ut(n,o);a(null,t(f,r,o))}catch(t){a(t)}}var lt=ut,vt=dt.bind(null,ft.render),pt=dt.bind(null,ft.renderToDataURL),wt=dt.bind(null,(function(t,r,e){return gt(t,e)})),mt={create:lt,toCanvas:vt,toDataURL:pt,toString:wt};return t.create=lt,t.default=mt,t.toCanvas=vt,t.toDataURL=pt,t.toString=wt,Object.defineProperty(t,"__esModule",{value:!0}),t}({});

const STYLE = `
.simplex-preview-container {
  --sp-bg: var(--sp-light-bg, #fff);
  --sp-text: #000;
  --sp-text-secondary: #8b8786;
  --sp-text-muted: #333;
  --sp-text-small: #888;
  --sp-bubble: #f5f5f6;
  --sp-quote: #ececee;
  --sp-border: #e5e5e5;
  --sp-link: #0088ff;
  --sp-link-hover: #0077e0;
  --sp-btn: #007AE5;
  --sp-btn-hover: #006BC9;
  --sp-color-blue: #0053d0;
  --sp-color-black: #000;
  --sp-color-white: #000;
  --sp-qr-fg: #062D56;
  --sp-qr-bg: #ffffff;
  font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Helvetica, Arial, sans-serif;
  font-size: 15px;
  line-height: 1.4;
  color: var(--sp-text);
  background: var(--sp-bg);
  width: 100%;
  height: 100%;
  padding: 0;
  -webkit-font-smoothing: antialiased;
  -webkit-text-size-adjust: 100%;
  text-size-adjust: 100%;
  display: flex;
  justify-content: center;
}

.simplex-preview-container.simplex-scheme-dark,
.dark .simplex-preview-container.simplex-scheme-site {
  --sp-bg: var(--sp-dark-bg, #000832);
  --sp-text: #FFFBFA;
  --sp-text-secondary: #B3AFAE;
  --sp-text-muted: #B3AFAE;
  --sp-text-small: #aaa;
  --sp-bubble: #071C46;
  --sp-quote: #1B325C;
  --sp-border: #3A3A3C;
  --sp-link: #70F0F9;
  --sp-link-hover: #66D9E2;
  --sp-btn: #7EF1F9;
  --sp-btn-hover: #75DCE4;
  --sp-btn-text: #000;
  --sp-color-blue: #70F0F9;
  --sp-color-black: #fff;
  --sp-color-white: #fff;
  --sp-qr-fg: #FFFBFA;
  --sp-qr-bg: transparent;
}

.simplex-preview-header {
  position: sticky;
  top: 0;
  z-index: 10;
  background: var(--sp-bg);
  border-bottom: 1px solid var(--sp-border);
  padding: 8px 16px;
  display: flex;
  align-items: center;
  gap: 12px;
}

.simplex-preview-header-avatar {
  width: 36px;
  height: 36px;
  border-radius: 8px;
  object-fit: cover;
  flex-shrink: 0;
}

.simplex-preview-header-info {
  flex: 1;
  min-width: 0;
}

.simplex-preview-header-name {
  font-size: 17px;
  font-weight: 600;
  margin: 0;
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
}

.simplex-preview-header-description {
  font-size: 13px;
  color: var(--sp-text-secondary);
  margin: 2px 0 0;
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
}

.simplex-preview-join-btn {
  flex-shrink: 0;
  background: var(--sp-btn);
  color: var(--sp-btn-text, #fff);
  border: none;
  border-radius: 34px;
  padding: 6px 10px 6px 10px;
  font-size: 14px;
  font-weight: 600;
  cursor: pointer;
  text-decoration: none;
  display: inline-flex;
  align-items: center;
  gap: 5px;
  font-family: inherit;
}

.simplex-preview-join-btn svg {
  width: 15.4px;
  height: 15.4px;
  flex-shrink: 0;
  margin-left: 2px;
}

.simplex-preview-container .simplex-logo-light-bg {
  display: none;
}

.simplex-preview-container.simplex-scheme-dark .simplex-logo-dark-bg,
.dark .simplex-preview-container.simplex-scheme-site .simplex-logo-dark-bg {
  display: none;
}

.simplex-preview-container.simplex-scheme-dark .simplex-logo-light-bg,
.dark .simplex-preview-container.simplex-scheme-site .simplex-logo-light-bg {
  display: inline;
}

.simplex-preview-join-btn:hover {
  background: var(--sp-btn-hover);
}

.simplex-preview-messages {
  padding: 8px 16px 32px;
}

.simplex-preview-date-separator {
  text-align: center;
  padding: 8px 0;
  font-size: 12px;
  color: var(--sp-text-secondary);
  font-weight: 500;
}

.simplex-preview-msg-group {
  padding: 0 8px;
}

.simplex-preview-msg-name {
  font-size: 13.5px;
  color: var(--sp-text-secondary);
  padding: 0 0 2px 0;
  margin-left: 39px;
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
}

.simplex-preview-msg-name-role {
  font-weight: 500;
  margin-left: 8px;
}

.simplex-preview-msg-row {
  display: flex;
  align-items: flex-start;
  margin-bottom: 2px;
}

.simplex-preview-msg-row.has-gap {
  margin-bottom: 6px;
}

.simplex-preview-msg-avatar {
  width: 30px;
  height: 30px;
  border-radius: 7px;
  object-fit: cover;
  flex-shrink: 0;
  margin-right: 9px;
}

.simplex-preview-msg-avatar-placeholder {
  width: 30px;
  flex-shrink: 0;
  margin-right: 9px;
}

.simplex-preview-bubble {
  position: relative;
  background: var(--sp-bubble);
  border-radius: 18px;
  min-width: 80px;
  overflow: visible;
}

.simplex-preview-bubble-inner {
  border-radius: 18px;
  overflow: hidden;
}

.simplex-preview-bubble.has-tail {
  border-bottom-left-radius: 0;
}

.simplex-preview-bubble.has-tail .simplex-preview-bubble-inner {
  border-bottom-left-radius: 0;
}

.simplex-preview-bubble-tail {
  position: absolute;
  bottom: 0;
  left: -9px;
  width: 9px;
  height: 16px;
  color: var(--sp-bubble);
}

.simplex-preview-bubble.media-only {
  background: transparent;
}

.simplex-preview-meta-overlay {
  position: absolute;
  bottom: 6px;
  right: 12px;
  font-size: 12px;
  color: #fff;
  text-shadow: 0 0 4px rgba(0,0,0,0.7), 0 0 2px rgba(0,0,0,0.9);
  white-space: nowrap;
}

.simplex-preview-meta-overlay .simplex-preview-meta-edited {
  font-style: italic;
}

.simplex-preview-forwarded-header {
  background: var(--sp-quote);
  padding: 6px 12px 6px 8px;
  font-size: 12px;
  font-style: italic;
  color: var(--sp-text-secondary);
  display: flex;
  align-items: center;
  gap: 4px;
}

.simplex-preview-quote {
  background: var(--sp-quote);
  display: flex;
  width: 100%;
}

.simplex-preview-quote-content {
  flex: 1;
  padding: 6px 12px;
  min-width: 0;
}

.simplex-preview-quote-sender {
  font-size: 13.5px;
  color: var(--sp-text-secondary);
  margin-bottom: 2px;
}

.simplex-preview-quote-text {
  font-size: 15px;
  overflow: hidden;
  display: -webkit-box;
  -webkit-line-clamp: 2;
  -webkit-box-orient: vertical;
}

.simplex-preview-quote-thumb {
  width: 68px;
  height: 68px;
  object-fit: cover;
  flex-shrink: 0;
}

.simplex-preview-quote-file-icon {
  padding: 6px 4px 0 0;
  flex-shrink: 0;
  color: var(--sp-text-secondary);
}

.simplex-preview-text {
  padding: 7px 12px;
  word-wrap: break-word;
  overflow-wrap: break-word;
}

.simplex-preview-text a {
  color: var(--sp-link);
  text-decoration: none;
}

.simplex-preview-text a:hover {
  text-decoration: underline;
}

.simplex-preview-image {
  display: block;
  max-width: 100%;
}

.simplex-preview-image.landscape {
  width: 400px;
}

.simplex-preview-image.portrait {
  width: 300px;
}

.simplex-preview-image-placeholder {
  display: flex;
  align-items: center;
  justify-content: center;
  width: 120px;
  height: 80px;
  background: var(--sp-quote);
  border-radius: 12px;
  color: var(--sp-text-secondary);
}

.simplex-preview-image-placeholder svg {
  width: 32px;
  height: 32px;
}

.simplex-preview-link-card {
  display: block;
  max-width: 400px;
}

.simplex-preview-link-card-image {
  display: block;
  width: 100%;
}

.simplex-preview-link-card-body {
  padding: 6px 12px;
}

.simplex-preview-link-card-title {
  font-size: 15px;
  line-height: 22px;
  margin-bottom: 4px;
  overflow: hidden;
  display: -webkit-box;
  -webkit-line-clamp: 3;
  -webkit-box-orient: vertical;
}

.simplex-preview-link-card-description {
  font-size: 14px;
  line-height: 20px;
  color: var(--sp-text-muted);
  overflow: hidden;
  display: -webkit-box;
  -webkit-line-clamp: 12;
  -webkit-box-orient: vertical;
}

.simplex-preview-link-card-uri {
  font-size: 12px;
  color: var(--sp-text-secondary);
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
}

.simplex-preview-file-indicator {
  padding: 7px 12px;
  display: flex;
  align-items: center;
  gap: 8px;
  color: var(--sp-text-secondary);
}

.simplex-preview-file-icon {
  width: 22px;
  height: 22px;
  flex-shrink: 0;
}

.simplex-preview-file-name {
  font-size: 14px;
  color: var(--sp-text);
}

.simplex-preview-file-size {
  font-size: 12px;
  color: var(--sp-text-secondary);
}

.simplex-preview-voice {
  padding: 7px 12px;
  display: flex;
  align-items: center;
  gap: 8px;
  color: var(--sp-text-secondary);
  font-size: 14px;
}

.simplex-preview-meta {
  float: right;
  font-size: 12px;
  color: var(--sp-text-secondary);
  padding: 0 2px 0 12px;
  margin-top: 4px;
  white-space: nowrap;
}

.simplex-preview-meta-edited {
  font-style: italic;
}

.simplex-preview-reactions {
  display: flex;
  flex-wrap: wrap;
  padding: 2px 5px 2px;
}

.simplex-preview-reaction {
  font-size: 12px;
  font-family: "Apple Color Emoji", "Segoe UI Emoji", "Noto Color Emoji", sans-serif;
  border-radius: 8px;
  padding: 2px 5px;
  display: inline-flex;
  align-items: center;
  gap: 4px;
}

.simplex-preview-reaction-count {
  font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Helvetica, Arial, sans-serif;
  color: var(--sp-text-secondary);
  font-size: 11.5px;
}

.simplex-preview-empty {
  text-align: center;
  padding: 48px 16px;
  color: var(--sp-text-secondary);
}

.simplex-preview-text .secret {
  background: var(--sp-text-secondary);
  color: transparent;
  border-radius: 4px;
  cursor: pointer;
  user-select: none;
  transition: all 0.2s;
}

.simplex-preview-text .secret.visible {
  background: transparent;
  color: inherit;
}

.simplex-preview-text .small-text {
  font-size: 13px;
  color: var(--sp-text-small);
}

.simplex-preview-text .red { color: #DD0000; }
.simplex-preview-text .green { color: #20BD3D; }
.simplex-preview-text .blue { color: var(--sp-color-blue); }
.simplex-preview-text .yellow { color: #DEBD00; }
.simplex-preview-text .cyan { color: #0AC4D1; }
.simplex-preview-text .magenta { color: magenta; }
.simplex-preview-text .black { color: var(--sp-color-black); }
.simplex-preview-text .white { color: var(--sp-color-white); }

.simplex-preview-main {
  flex: 1;
  min-width: 0;
  max-width: 640px;
  overflow-y: auto;
  overscroll-behavior: contain;
  position: relative;
}

.simplex-preview-info {
  overflow-y: auto;
  overscroll-behavior: contain;
  background: var(--sp-bg);
}

.simplex-preview-info-close {
  display: none;
}

.simplex-preview-info-avatar {
  width: 192px;
  height: 192px;
  border-radius: 42px;
  object-fit: cover;
  display: block;
  margin: 12px auto;
}

.simplex-preview-info-name {
  font-size: 34px;
  font-weight: 700;
  text-align: center;
  margin: 0;
}

.simplex-preview-info-descr {
  font-size: 14px;
  color: var(--sp-text-secondary);
  text-align: center;
  margin: 8px 0;
  word-wrap: break-word;
  overflow-wrap: break-word;
}

.simplex-preview-info-descr a {
  color: var(--sp-link);
  text-decoration: none;
}

.simplex-preview-info-descr a:hover {
  text-decoration: underline;
}

.simplex-preview-info-subscribers {
  font-size: 14px;
  color: var(--sp-text-secondary);
  text-align: center;
  margin: 0 0 16px;
}

.simplex-preview-info .simplex-preview-join-btn {
  display: block;
  text-align: center;
  margin-top: 20px;
  width: 100%;
}

.simplex-preview-conversion {
  display: flex;
  flex-direction: column;
  align-items: center;
  gap: 16px;
}

.simplex-preview-divider {
  width: 100%;
  height: 1px;
  background: var(--sp-border);
  margin: 40px 0;
}

.simplex-preview-conversion-title {
  font-size: 18px;
  font-weight: 600;
  text-align: center;
  margin: 0 0 16px;
}

.simplex-preview-qr-toggle {
  font-size: 14px;
  color: var(--sp-link);
  cursor: pointer;
  text-decoration: none;
}

.simplex-preview-qr-toggle:hover {
  text-decoration: underline;
}

.simplex-preview-qr-popup {
  flex-direction: column;
  align-items: center;
  gap: 8px;
}

.simplex-preview-qr-popup canvas {
  border-radius: 8px;
}

.simplex-preview-qr-caption {
  font-size: 14px;
  text-align: center;
  margin: 0;
}

.simplex-preview-badges {
  display: flex;
  align-items: center;
  justify-content: center;
  gap: 8px;
  flex-wrap: wrap;
  margin: 0 0 6px;
}

.simplex-preview-badges a {
  display: block;
}

.simplex-preview-badges a img {
  height: 40px;
  width: auto;
  display: block;
}

.simplex-preview-copy-action {
  font-size: 14px;
  margin: 0;
}

.simplex-preview-copy-action a {
  color: var(--sp-link);
  text-decoration: none;
  cursor: pointer;
}

.simplex-preview-copy-action a:hover {
  text-decoration: underline;
}

.simplex-preview-step-title {
  font-size: 14px;
  text-align: center;
  margin: 0 0 -8px;
}

.simplex-preview-open-btn {
  display: inline-flex;
  align-items: center;
  justify-content: center;
  gap: 6px;
  background: var(--sp-btn);
  color: var(--sp-btn-text, #fff);
  border: none;
  border-radius: 34px;
  padding: 16px 12px 16px 18px;
  height: 44px;
  font-size: 16px;
  line-height: 19px;
  letter-spacing: 0.02em;
  cursor: pointer;
  text-decoration: none;
  font-family: inherit;
  margin-top: 3px;
}

.simplex-preview-open-btn svg {
  width: 22px;
  height: 22px;
  flex-shrink: 0;
  margin-left: 6px;
}


.simplex-preview-open-btn:hover {
  background: var(--sp-btn-hover);
}

@media (min-width: 1000px) {
  .simplex-preview-info {
    width: 320px;
    flex-shrink: 0;
    border-left: 1px solid var(--sp-border);
    padding: 24px;
  }
  .simplex-preview-header .simplex-preview-join-btn {
    display: none;
  }
}

@media (max-width: 999px) {
  .simplex-preview-container {
    font-size: 17px;
  }
  .simplex-preview-main {
    max-width: none;
  }
  .simplex-preview-info {
    display: none;
    position: fixed;
    top: 0;
    left: 0;
    right: 0;
    bottom: 0;
    z-index: 100;
    padding: 16px;
  }
  .simplex-preview-info.open {
    display: block;
  }
  .simplex-preview-info-close {
    display: block;
    position: absolute;
    top: 12px;
    right: 12px;
    background: none;
    border: none;
    font-size: 24px;
    color: var(--sp-text-secondary);
    cursor: pointer;
    padding: 4px 8px;
    line-height: 1;
  }
  .simplex-preview-info-content {
    padding-top: 32px;
  }
  .simplex-preview-header {
    cursor: pointer;
  }
}
`;

const DEFAULT_AVATAR = 'data:image/svg+xml,' + encodeURIComponent('<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 25.8 25.46"><circle cx="12.9" cy="12.73" r="12.72" fill="#eee"/><path d="M25.44 12.73c0 7.01-5.71 12.73-12.73 12.73C5.71 25.46 0 19.75 0 12.73 0 5.73 5.71.02 12.71.02c7.01 0 12.73 5.71 12.73 12.72zM4.22 17.11c0 .58.31.87 1.05.87h4.88c-.14-.26-.19-.55-.19-.82 0-1.03.59-2.19 1.63-3.1-.79-.5-1.75-.79-2.74-.79-2.37 0-4.63 1.68-4.63 3.84zm6.6.05c0 .56.36.82 1.27.82h8.1c.93 0 1.26-.26 1.26-.82 0-1.62-2.02-3.87-5.28-3.87-3.29 0-5.34 2.25-5.34 3.87zM6.66 9.87c0 1.37 1.01 2.42 2.19 2.42 1.2 0 2.2-1.05 2.2-2.43 0-1.33-1.02-2.36-2.2-2.36-1.16 0-2.19 1.05-2.19 2.37zm6.98-.5c0 1.56 1.13 2.78 2.52 2.78 1.36 0 2.5-1.22 2.5-2.79 0-1.53-1.15-2.71-2.5-2.71-1.38 0-2.52 1.21-2.52 2.73z" fill="#ccc" fill-opacity=".85"/></svg>');

const IMAGE_PLACEHOLDER_SVG = `<svg class="simplex-preview-file-icon" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.5"><rect x="3" y="3" width="18" height="18" rx="2"/><circle cx="8.5" cy="8.5" r="1.5"/><path d="M21 15l-5-5L5 21"/></svg>`;

function isDataImage(src) {
  return typeof src === 'string' && src.startsWith('data:image/');
}

function tailSvg() {
  return '<svg width="9" height="16" viewBox="0 0 9 16" xmlns="http://www.w3.org/2000/svg"><path d="M9 0 L9 16 L0 16 Q9 11 9 0 Z" fill="currentColor"/></svg>';
}

var _logoId = 0;
var _svgParser = new DOMParser();

function appendSimplexLogo(el) {
  var n = _logoId++;
  var darkSvg = '<svg class="simplex-logo-dark-bg" viewBox="0 0 34 35" fill="none" xmlns="http://www.w3.org/2000/svg">'
    + '<path fill-rule="evenodd" clip-rule="evenodd" d="M3.03 8.61l5.592 5.592 5.748-5.747 2.797 2.796-5.749 5.747 5.593 5.593-2.874 2.874-5.593-5.593-5.748 5.748L0 22.823l5.748-5.748L.155 11.483l2.874-2.874z" fill="white"/>'
    + '<path fill-rule="evenodd" clip-rule="evenodd" d="M14.092 25.516l2.852-2.852-.001-.001 5.704-5.702-5.595-5.594h.001l-2.798-2.797L8.66 2.975l2.852-2.851 5.594 5.594L22.81.016l2.797 2.797-5.704 5.703 5.596 5.594 5.704-5.703 2.797 2.797-5.704 5.703 5.596 5.595-2.852 2.851-5.595-5.594-5.704 5.703 5.596 5.594-2.852 2.852-5.595-5.595-5.703 5.703-2.798-2.797 5.704-5.703z" fill="url(#sp-lg-d' + n + ')"/>'
    + '<defs><linearGradient id="sp-lg-d' + n + '" x1="12.838" y1="-0.678" x2="9.544" y2="31.449" gradientUnits="userSpaceOnUse"><stop stop-color="#01F1FF"/><stop offset="1" stop-color="#0197FF"/></linearGradient></defs></svg>';
  var lightSvg = '<svg class="simplex-logo-light-bg" viewBox="0 0 34 34" fill="none" xmlns="http://www.w3.org/2000/svg">'
    + '<path fill-rule="evenodd" clip-rule="evenodd" d="M3.03 8.594l5.592 5.592 5.748-5.748 2.797 2.797-5.749 5.747 5.593 5.593-2.874 2.874-5.593-5.593-5.748 5.748L0 22.807l5.748-5.748L.156 11.468l2.874-2.874z" fill="#023789"/>'
    + '<path fill-rule="evenodd" clip-rule="evenodd" d="M14.092 25.5l2.851-2.851-.001-.001 5.704-5.702-5.595-5.594h.001l-2.798-2.797L8.66 2.96l2.851-2.852 5.595 5.595L22.81 0l2.797 2.797-5.704 5.703 5.596 5.594 5.704-5.703 2.797 2.797-5.704 5.703 5.596 5.595-2.852 2.851-5.595-5.594-5.704 5.703 5.596 5.595-2.852 2.851-5.595-5.594L5.386 34l-2.797-2.797 5.703-5.703z" fill="url(#sp-lg-l' + n + ')"/>'
    + '<defs><linearGradient id="sp-lg-l' + n + '" x1="12.838" y1="-0.694" x2="9.543" y2="31.434" gradientUnits="userSpaceOnUse"><stop stop-color="#01F1FF"/><stop offset="1" stop-color="#0197FF"/></linearGradient></defs></svg>';
  el.appendChild(document.importNode(_svgParser.parseFromString(darkSvg, 'image/svg+xml').documentElement, true));
  el.appendChild(document.importNode(_svgParser.parseFromString(lightSvg, 'image/svg+xml').documentElement, true));
}

const FILE_ICON_SVG = `<svg class="simplex-preview-file-icon" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><path d="M14 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V8z"/><polyline points="14,2 14,8 20,8"/></svg>`;

const VOICE_ICON_SVG = `<svg class="simplex-preview-file-icon" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><path d="M12 1a3 3 0 0 0-3 3v8a3 3 0 0 0 6 0V4a3 3 0 0 0-3-3z"/><path d="M19 10v2a7 7 0 0 1-14 0v-2"/><line x1="12" y1="19" x2="12" y2="23"/></svg>`;

const FORWARD_ICON_SVG = `<svg width="14" height="14" viewBox="0 -960 960 960" fill="currentColor"><path transform="scale(-1,1) translate(-960,0)" d="m236.5-495.5 142.5 143q8.5 8.5 8.25 20.25T378.5-312q-9 8.5-21 8.5t-20.5-9L145.5-504q-9-8.5-9-20t9-20.5L338-737q9-9 20.75-8.75T379.5-737q8.5 8.5 8.5 20.5t-8.5 20.5l-143 143h403q84 0 139.75 56T835-357.5V-234q0 12.5-8.25 20.75T806.5-205q-12.5 0-20.75-8.25T777.5-234v-123.5q0-60-39-99t-99-39h-403Z"/></svg>`;

const COPY_ICON_SVG = `<svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><rect x="9" y="9" width="13" height="13" rx="2"/><path d="M5 15H4a2 2 0 0 1-2-2V4a2 2 0 0 1 2-2h9a2 2 0 0 1 2 2v1"/></svg>`;

function initChannelPreview(container) {
  const relayDomains = (container.dataset.relayDomains || '').split(',').map(u => u.trim()).filter(Boolean);
  const relayScheme = container.dataset.relayScheme || 'https';
  const channelId = container.dataset.channelId || '';
  const channelLink = container.dataset.channelLink || '';
  const showAppBadges = container.dataset.appDownloadButtons !== 'off';
  const colorScheme = container.dataset.colorScheme || 'light';

  if (!relayDomains.length || !channelId) {
    container.innerHTML = '<p class="simplex-preview-empty">Missing configuration: data-relay-domains and data-channel-id required.</p>';
    return;
  }

  injectStyles();
  container.classList.add('simplex-preview-container', 'simplex-scheme-' + colorScheme);
  if (container.dataset.lightBackground) {
    container.style.setProperty('--sp-light-bg', container.dataset.lightBackground);
  }
  if (container.dataset.darkBackground) {
    container.style.setProperty('--sp-dark-bg', container.dataset.darkBackground);
  }
  container.innerHTML = '<p class="simplex-preview-empty">Loading channel...</p>';

  fetchPreview(relayScheme, relayDomains, channelLink, channelId).then(data => {
    if (data === 'link_mismatch') {
      container.innerHTML = '<p class="simplex-preview-empty">All relays returned a different channel link from specified in the page.</p>';
      return;
    }
    if (!data) {
      container.innerHTML = '<p class="simplex-preview-empty">Failed to load channel preview.</p>';
      return;
    }
    render(container, data, channelLink, showAppBadges);
  });
}

let stylesInjected = false;
function injectStyles() {
  if (stylesInjected) return;
  stylesInjected = true;
  const style = document.createElement('style');
  style.textContent = STYLE;
  document.head.appendChild(style);
}

async function fetchPreview(relayScheme, relayDomains, channelLink, channelId) {
  let linkMismatch = false;
  for (const domain of relayDomains) {
    try {
      const url = `${relayScheme}://${domain}/channel/${channelId}.json`;
      const resp = await fetch(url);
      if (!resp.ok) continue;
      const data = await resp.json();
      const relayLink = data.channel?.publicGroup?.groupLink;
      if (channelLink && relayLink && channelLink !== relayLink) {
        linkMismatch = true;
        continue;
      }
      return data;
    } catch(e) {
      continue;
    }
  }
  return linkMismatch ? 'link_mismatch' : null;
}

function render(container, data, channelLink, showAppBadges) {
  const { channel, members, messages } = data;
  const membersMap = {};
  for (const m of members) {
    membersMap[m.memberId] = m;
  }

  container.innerHTML = '';

  const main = document.createElement('div');
  main.className = 'simplex-preview-main';

  const header = renderHeader(channel, channelLink, data.subscribers);
  main.appendChild(header);

  const messagesDiv = document.createElement('div');
  messagesDiv.className = 'simplex-preview-messages';
  const welcome = data.welcomeMessage || channel.description;
  var allMessages = messages;
  if (welcome) {
    var welcomeMsg = {
      sender: null,
      ts: messages.length > 0 ? messages[0].ts : new Date().toISOString(),
      content: { type: 'text', text: typeof welcome === 'string' ? welcome : '' },
      formattedText: Array.isArray(welcome) ? welcome : null,
      reactions: []
    };
    allMessages = [welcomeMsg].concat(messages);
  }
  renderMessages(messagesDiv, allMessages, membersMap, channel);
  main.appendChild(messagesDiv);

  container.appendChild(main);

  const info = document.createElement('div');
  info.className = 'simplex-preview-info';

  const closeBtn = document.createElement('button');
  closeBtn.className = 'simplex-preview-info-close';
  closeBtn.innerHTML = '&#10005;';
  info.appendChild(closeBtn);

  const infoContent = document.createElement('div');
  infoContent.className = 'simplex-preview-info-content';
  renderInfoContent(infoContent, data, channelLink, data.subscribers, showAppBadges);
  info.appendChild(infoContent);

  container.appendChild(info);

  header.addEventListener('click', (e) => {
    if (e.target.closest('.simplex-preview-join-btn')) return;
    if (window.innerWidth < 1000) {
      info.classList.add('open');
      main.style.overflow = 'hidden';
    }
  });

  closeBtn.addEventListener('click', () => {
    info.classList.remove('open');
    main.style.overflow = '';
  });

  setupSecretToggles(container);
  setTimeout(() => { main.scrollTop = main.scrollHeight; }, 0);
}

function renderHeader(channel, channelLink, subscriberCount) {
  const header = document.createElement('div');
  header.className = 'simplex-preview-header';

  const avatar = document.createElement('img');
  avatar.className = 'simplex-preview-header-avatar';
  avatar.src = isDataImage(channel.image) ? channel.image : DEFAULT_AVATAR;
  avatar.alt = channel.displayName;
  header.appendChild(avatar);

  const info = document.createElement('div');
  info.className = 'simplex-preview-header-info';

  const name = document.createElement('h1');
  name.className = 'simplex-preview-header-name';
  name.textContent = channel.displayName;
  info.appendChild(name);

  if (subscriberCount > 0) {
    const desc = document.createElement('p');
    desc.className = 'simplex-preview-header-description';
    desc.textContent = subscriberCount + ' subscribers';
    info.appendChild(desc);
  }

  header.appendChild(info);

  if (channelLink) {
    const btn = document.createElement('a');
    btn.className = 'simplex-preview-join-btn';
    btn.textContent = 'Join';
    appendSimplexLogo(btn);
    btn.href = channelLink;
    header.appendChild(btn);
  }

  return header;
}

function renderInfoContent(container, data, channelLink, subscriberCount, showAppBadges) {
  const { channel } = data;

  const avatar = document.createElement('img');
  avatar.className = 'simplex-preview-info-avatar';
  avatar.src = isDataImage(channel.image) ? channel.image : DEFAULT_AVATAR;
  avatar.alt = channel.displayName;
  container.appendChild(avatar);

  const name = document.createElement('h2');
  name.className = 'simplex-preview-info-name';
  name.textContent = channel.displayName;
  container.appendChild(name);

  const shortDescr = data.shortDescription || channel.shortDescr;
  if (shortDescr) {
    const descrDiv = document.createElement('div');
    descrDiv.className = 'simplex-preview-info-descr';
    descrDiv.innerHTML = Array.isArray(shortDescr) ? renderMarkdown(shortDescr) : escapeHtml(shortDescr);
    container.appendChild(descrDiv);
  }

  if (subscriberCount > 0) {
    const subs = document.createElement('p');
    subs.className = 'simplex-preview-info-subscribers';
    subs.textContent = subscriberCount + ' subscribers';
    container.appendChild(subs);
  }

  if (channelLink) {
    if (!isMobile.any()) {
      const openBtn = document.createElement('a');
      openBtn.className = 'simplex-preview-open-btn';
      openBtn.style.display = 'flex';
      openBtn.style.width = 'fit-content';
      openBtn.style.margin = '32px auto 0';
      openBtn.textContent = 'Join in SimpleX Chat';
      appendSimplexLogo(openBtn);
      openBtn.href = channelLink;
      container.appendChild(openBtn);
    }

    const showJoinSection = !isMobile.any() || showAppBadges;
    if (showJoinSection) {
      const divider = document.createElement('div');
      divider.className = 'simplex-preview-divider';
      container.appendChild(divider);

      const joinTitle = document.createElement('p');
      joinTitle.className = 'simplex-preview-conversion-title';
      joinTitle.textContent = 'To join this channel';
      container.appendChild(joinTitle);
    }

    const conversion = document.createElement('div');
    conversion.className = 'simplex-preview-conversion';
    if (!showJoinSection) {
      conversion.style.marginTop = '28px';
    }
    if (isMobile.any()) {
      renderMobileConversion(conversion, channelLink, showAppBadges);
    } else {
      renderDesktopConversion(conversion, channelLink, showAppBadges);
    }
    container.appendChild(conversion);
  }
}

var BADGE_APPLE = '<a href="https://apps.apple.com/us/app/simplex-chat/id1605771084" target="_blank"><img src="https://simplex.chat/img/new/apple_store.svg" alt="App Store"></a>';
var BADGE_GOOGLE = '<a href="https://play.google.com/store/apps/details?id=chat.simplex.app" target="_blank"><img src="https://simplex.chat/img/new/google_play.svg" alt="Google Play"></a>';
var BADGE_FDROID = '<a href="https://simplex.chat/fdroid" target="_blank"><img src="https://simplex.chat/img/new/f_droid.svg" alt="F-Droid"></a>';
var BADGE_APK = '<a href="https://github.com/simplex-chat/simplex-chat/releases/latest/download/simplex-aarch64.apk" target="_blank"><img src="https://simplex.chat/img/design_3/android-dark.png" alt="APK Download"></a>';
var BADGE_TESTFLIGHT = '<a href="https://testflight.apple.com/join/DWuT2LQu" target="_blank"><img src="https://simplex.chat/img/design_3/testflight-dark.png" alt="TestFlight"></a>';

function renderAppBadges(container) {
  const title = document.createElement('p');
  title.className = 'simplex-preview-step-title';
  title.textContent = 'Install SimpleX Chat app';
  container.appendChild(title);

  const badges = document.createElement('div');
  badges.className = 'simplex-preview-badges';
  if (isMobile.Android()) {
    badges.innerHTML = BADGE_GOOGLE + BADGE_FDROID + BADGE_APK;
  } else if (isMobile.iOS()) {
    badges.innerHTML = BADGE_APPLE + BADGE_TESTFLIGHT;
  } else {
    badges.innerHTML = BADGE_APPLE + BADGE_GOOGLE;
  }
  container.appendChild(badges);
}

function renderDesktopConversion(container, channelLink, showAppBadges) {
  if (showAppBadges) {
    renderAppBadges(container);
  }

  const qrToggle = document.createElement('a');
  qrToggle.className = 'simplex-preview-qr-toggle';
  qrToggle.textContent = 'Show QR code for mobile app';
  qrToggle.href = '#';
  container.appendChild(qrToggle);

  const qrPopup = document.createElement('div');
  qrPopup.className = 'simplex-preview-qr-popup';
  qrPopup.style.display = 'none';

  const caption = document.createElement('p');
  caption.className = 'simplex-preview-qr-caption';
  caption.textContent = 'Scan from SimpleX Chat app';
  qrPopup.appendChild(caption);

  const canvas = document.createElement('canvas');
  qrPopup.appendChild(canvas);

  const qrHide = document.createElement('a');
  qrHide.className = 'simplex-preview-qr-toggle';
  qrHide.textContent = 'Hide QR code';
  qrHide.href = '#';
  qrPopup.appendChild(qrHide);
  container.appendChild(qrPopup);

  function toggleQr(e) {
    e.preventDefault();
    if (qrPopup.style.display === 'none') {
      qrPopup.style.display = 'flex';
      qrToggle.style.display = 'none';
      if (!canvas._rendered) {
        canvas._rendered = true;
        try {
          var cs = getComputedStyle(container);
          QRCode.toCanvas(canvas, channelLink, {
            errorCorrectionLevel: 'M',
            color: {
              dark: cs.getPropertyValue('--sp-qr-fg').trim() || '#062D56',
              light: cs.getPropertyValue('--sp-qr-bg').trim() || '#ffffff'
            },
            width: 400,
            margin: 1
          }).then(function() {
            canvas.style.width = '200px';
            canvas.style.height = '200px';
          }).catch(function() {
            qrPopup.style.display = 'none';
            qrToggle.style.display = 'none';
          });
        } catch(err) {
          qrPopup.style.display = 'none';
          qrToggle.style.display = 'none';
        }
      }
    } else {
      qrPopup.style.display = 'none';
      qrToggle.style.display = '';
    }
  }
  qrToggle.addEventListener('click', toggleQr);
  qrHide.addEventListener('click', toggleQr);

  const copyAction = document.createElement('p');
  copyAction.className = 'simplex-preview-copy-action';
  const copyLink = document.createElement('a');
  copyLink.textContent = 'copy link';
  copyLink.addEventListener('click', function() {
    navigator.clipboard.writeText(channelLink).then(function() {
      copyLink.textContent = 'Copied!';
      setTimeout(function() { copyLink.textContent = 'copy link'; }, 2000);
    });
  });
  copyAction.appendChild(document.createTextNode('Or '));
  copyAction.appendChild(copyLink);
  copyAction.appendChild(document.createTextNode(' for desktop app'));
  container.appendChild(copyAction);
}

function renderMobileConversion(container, channelLink, showAppBadges) {
  if (showAppBadges) {
    renderAppBadges(container);
  }

  const openBtn = document.createElement('a');
  openBtn.className = 'simplex-preview-open-btn';
  openBtn.textContent = 'Join in SimpleX Chat';
  appendSimplexLogo(openBtn);
  openBtn.href = channelLink;
  container.appendChild(openBtn);
}


function setupSecretToggles(container) {
  container.addEventListener('click', (e) => {
    const secret = e.target.closest('.secret');
    if (secret) secret.classList.toggle('visible');
  });
}

function renderMessages(container, messages, membersMap, channel) {
  const hasAnySender = messages.some(function(m) { return m.sender; });
  let prevMsg = null;
  let prevDate = null;

  for (let i = 0; i < messages.length; i++) {
    const msg = messages[i];
    const nextMsg = i < messages.length - 1 ? messages[i + 1] : null;

    const msgDate = formatDateLabel(msg.ts);
    if (msgDate !== prevDate) {
      const dateSep = document.createElement('div');
      dateSep.className = 'simplex-preview-date-separator';
      dateSep.textContent = msgDate;
      container.appendChild(dateSep);
      prevDate = msgDate;
    }

    const separation = getItemSeparation(msg, prevMsg);
    const nextSeparation = getItemSeparation(nextMsg, msg);
    const showAvatar = hasAnySender && (!prevMsg || msg.sender !== prevMsg.sender);
    const showName = showAvatar;
    const showTail = nextSeparation.largeGap;

    const member = msg.sender ? membersMap[msg.sender] : null;
    const senderName = member ? member.displayName : channel.displayName;
    const senderImage = member ? member.image : channel.image;

    const row = document.createElement('div');
    row.className = 'simplex-preview-msg-row' + (nextSeparation.largeGap ? ' has-gap' : '');

    if (hasAnySender) {
      if (showName) {
        const nameDiv = document.createElement('div');
        nameDiv.className = 'simplex-preview-msg-name';
        nameDiv.textContent = senderName;
        container.appendChild(nameDiv);
      }

      if (showAvatar) {
        const avatarImg = document.createElement('img');
        avatarImg.className = 'simplex-preview-msg-avatar';
        avatarImg.src = isDataImage(senderImage) ? senderImage : DEFAULT_AVATAR;
        avatarImg.alt = senderName;
        row.appendChild(avatarImg);
      } else {
        const spacer = document.createElement('div');
        spacer.className = 'simplex-preview-msg-avatar-placeholder';
        row.appendChild(spacer);
      }
    }

    const col = document.createElement('div');
    const bubble = renderBubble(msg, member, showTail, membersMap, channel);
    col.appendChild(bubble);

    if (msg.reactions && msg.reactions.length > 0) {
      col.appendChild(renderReactions(msg.reactions));
    }

    row.appendChild(col);
    container.appendChild(row);
    prevMsg = msg;
  }
}

function renderBubble(msg, member, showTail, membersMap, channel) {
  const mc = msg.content;
  const mediaOnly = (mc.type === 'image' || mc.type === 'video') && !mc.text && !msg.quote && !msg.forward;
  const noTailContent = (mc.type === 'image' || mc.type === 'video' || mc.type === 'voice') && !mc.text;
  const hasTail = showTail && !noTailContent;

  const bubble = document.createElement('div');
  bubble.className = 'simplex-preview-bubble' + (hasTail ? ' has-tail' : '') + (mediaOnly ? ' media-only' : '');

  if (hasTail) {
    const tail = document.createElement('div');
    tail.className = 'simplex-preview-bubble-tail';
    tail.innerHTML = tailSvg();
    bubble.appendChild(tail);
  }

  const inner = document.createElement('div');
  inner.className = 'simplex-preview-bubble-inner';

  if (msg.forward) {
    const fwd = document.createElement('div');
    fwd.className = 'simplex-preview-forwarded-header';
    fwd.innerHTML = FORWARD_ICON_SVG + ' <span>Forwarded</span>';
    inner.appendChild(fwd);
  }

  if (msg.quote) {
    inner.appendChild(renderQuote(msg.quote, membersMap, channel));
  }

  switch (mc.type) {
    case 'image':
      renderImageContent(inner, mc, msg, mediaOnly);
      break;
    case 'video':
      renderVideoContent(inner, mc, msg, mediaOnly);
      break;
    case 'link':
      renderLinkContent(inner, mc, msg);
      break;
    case 'voice':
      renderVoiceContent(inner, mc, msg);
      break;
    case 'file':
      renderFileContent(inner, mc, msg);
      break;
    default:
      renderTextContent(inner, msg);
      break;
  }

  bubble.appendChild(inner);

  if (mediaOnly) {
    const overlay = document.createElement('div');
    overlay.className = 'simplex-preview-meta-overlay';
    if (msg.edited) overlay.innerHTML = '<span class="simplex-preview-meta-edited">edited </span>';
    overlay.innerHTML += formatTime(msg.ts);
    bubble.appendChild(overlay);
  }

  return bubble;
}

function renderQuote(quote, membersMap, channel) {
  const quoteDiv = document.createElement('div');
  quoteDiv.className = 'simplex-preview-quote';

  const contentDiv = document.createElement('div');
  contentDiv.className = 'simplex-preview-quote-content';

  const ref = quote.msgRef;
  let senderName = '';
  if (ref) {
    if (ref.memberId) {
      const quotedMember = membersMap[ref.memberId];
      senderName = quotedMember ? quotedMember.displayName : '';
    } else if (ref.sent) {
      senderName = channel.displayName;
    }
  }
  if (senderName) {
    const sender = document.createElement('div');
    sender.className = 'simplex-preview-quote-sender';
    sender.textContent = senderName;
    contentDiv.appendChild(sender);
  }

  const textDiv = document.createElement('div');
  textDiv.className = 'simplex-preview-quote-text';
  textDiv.textContent = quote.content ? (quote.content.text || '') : '';
  contentDiv.appendChild(textDiv);

  quoteDiv.appendChild(contentDiv);

  if (quote.content) {
    if ((quote.content.type === 'image' || quote.content.type === 'video') && isDataImage(quote.content.image)) {
      const thumb = document.createElement('img');
      thumb.className = 'simplex-preview-quote-thumb';
      thumb.src = quote.content.image;
      quoteDiv.appendChild(thumb);
    } else if (quote.content.type === 'file') {
      const icon = document.createElement('div');
      icon.className = 'simplex-preview-quote-file-icon';
      icon.innerHTML = FILE_ICON_SVG;
      quoteDiv.appendChild(icon);
    } else if (quote.content.type === 'voice') {
      const icon = document.createElement('div');
      icon.className = 'simplex-preview-quote-file-icon';
      icon.innerHTML = VOICE_ICON_SVG;
      quoteDiv.appendChild(icon);
    }
  }

  return quoteDiv;
}

function classifyImage(img) {
  const w = img.naturalWidth;
  const h = img.naturalHeight;
  img.classList.add(w * 0.97 <= h ? 'portrait' : 'landscape');
}

function renderImageContent(inner, mc, msg, mediaOnly) {
  if (isDataImage(mc.image)) {
    const img = document.createElement('img');
    img.className = 'simplex-preview-image';
    img.src = mc.image;
    img.alt = 'Image';
    img.addEventListener('load', () => classifyImage(img));
    inner.appendChild(img);
  } else {
    const ph = document.createElement('div');
    ph.className = 'simplex-preview-image-placeholder';
    ph.innerHTML = IMAGE_PLACEHOLDER_SVG;
    inner.appendChild(ph);
  }
  if (mc.text) {
    appendTextBlock(inner, msg);
  } else if (!mediaOnly) {
    appendMetaOnly(inner, msg);
  }
}

function renderVideoContent(inner, mc, msg, mediaOnly) {
  if (isDataImage(mc.image)) {
    const wrapper = document.createElement('div');
    wrapper.style.position = 'relative';
    const img = document.createElement('img');
    img.className = 'simplex-preview-image';
    img.src = mc.image;
    img.alt = 'Video';
    img.addEventListener('load', () => classifyImage(img));
    wrapper.appendChild(img);
    const dur = document.createElement('span');
    dur.style.cssText = 'position:absolute;bottom:6px;left:12px;color:#fff;font-size:12px;text-shadow:0 0 4px rgba(0,0,0,0.7),0 0 2px rgba(0,0,0,0.9);';
    dur.textContent = formatDuration(mc.duration || 0);
    wrapper.appendChild(dur);
    inner.appendChild(wrapper);
  } else {
    const ph = document.createElement('div');
    ph.className = 'simplex-preview-image-placeholder';
    ph.innerHTML = IMAGE_PLACEHOLDER_SVG;
    inner.appendChild(ph);
  }
  if (mc.text) {
    appendTextBlock(inner, msg);
  } else if (!mediaOnly) {
    appendMetaOnly(inner, msg);
  }
}

function renderLinkContent(bubble, mc, msg) {
  if (mc.preview) {
    const card = document.createElement('div');
    card.className = 'simplex-preview-link-card';
    if (isDataImage(mc.preview.image)) {
      const img = document.createElement('img');
      img.className = 'simplex-preview-link-card-image';
      img.src = mc.preview.image;
      img.alt = mc.preview.title || '';
      card.appendChild(img);
    }
    const body = document.createElement('div');
    body.className = 'simplex-preview-link-card-body';
    if (mc.preview.title) {
      const title = document.createElement('div');
      title.className = 'simplex-preview-link-card-title';
      title.textContent = mc.preview.title;
      body.appendChild(title);
    }
    if (mc.preview.description) {
      const desc = document.createElement('div');
      desc.className = 'simplex-preview-link-card-description';
      desc.textContent = mc.preview.description;
      body.appendChild(desc);
    }
    if (mc.preview.uri) {
      const uri = document.createElement('div');
      uri.className = 'simplex-preview-link-card-uri';
      uri.textContent = mc.preview.uri;
      body.appendChild(uri);
    }
    card.appendChild(body);
    bubble.appendChild(card);
  }
  appendTextBlock(bubble, msg);
}

function renderVoiceContent(bubble, mc, msg) {
  const voiceDiv = document.createElement('div');
  voiceDiv.className = 'simplex-preview-voice';
  voiceDiv.innerHTML = VOICE_ICON_SVG + ' <span>' + formatDuration(mc.duration || 0) + '</span>';
  bubble.appendChild(voiceDiv);
  if (mc.text) {
    appendTextBlock(bubble, msg);
  } else {
    appendMetaOnly(bubble, msg);
  }
}

function renderFileContent(bubble, mc, msg) {
  const fileDiv = document.createElement('div');
  fileDiv.className = 'simplex-preview-file-indicator';
  fileDiv.innerHTML = FILE_ICON_SVG;
  const info = document.createElement('div');
  if (msg.file) {
    const nameSpan = document.createElement('div');
    nameSpan.className = 'simplex-preview-file-name';
    nameSpan.textContent = msg.file.fileName;
    info.appendChild(nameSpan);
    const sizeSpan = document.createElement('div');
    sizeSpan.className = 'simplex-preview-file-size';
    sizeSpan.textContent = formatFileSize(msg.file.fileSize);
    info.appendChild(sizeSpan);
  }
  fileDiv.appendChild(info);
  bubble.appendChild(fileDiv);
  if (mc.text) {
    appendTextBlock(bubble, msg);
  } else {
    appendMetaOnly(bubble, msg);
  }
}

function renderTextContent(bubble, msg) {
  appendTextBlock(bubble, msg);
}

function appendTextBlock(bubble, msg) {
  const textDiv = document.createElement('div');
  textDiv.className = 'simplex-preview-text';
  const meta = renderMetaHTML(msg);
  if (msg.formattedText && msg.formattedText.length > 0) {
    textDiv.innerHTML = renderMarkdown(msg.formattedText) + meta;
  } else {
    textDiv.innerHTML = escapeHtml(msg.content.text || '') + meta;
  }
  bubble.appendChild(textDiv);
}

function appendMetaOnly(bubble, msg) {
  const metaDiv = document.createElement('div');
  metaDiv.style.cssText = 'padding: 0 8px 4px; text-align: right;';
  metaDiv.innerHTML = renderMetaHTML(msg);
  bubble.appendChild(metaDiv);
}

function renderMetaHTML(msg) {
  let html = '<span class="simplex-preview-meta">';
  if (msg.edited) html += '<span class="simplex-preview-meta-edited">edited </span>';
  html += formatTime(msg.ts);
  html += '</span>';
  return html;
}

function renderReactions(reactions) {
  const div = document.createElement('div');
  div.className = 'simplex-preview-reactions';
  for (const r of reactions) {
    if (r.totalReacted < 1) continue;
    const badge = document.createElement('span');
    badge.className = 'simplex-preview-reaction';
    const emoji = r.reaction && r.reaction.emoji ? r.reaction.emoji : '?';
    badge.appendChild(document.createTextNode(emoji));
    if (r.totalReacted > 1) {
      const count = document.createElement('span');
      count.className = 'simplex-preview-reaction-count';
      count.textContent = r.totalReacted;
      badge.appendChild(count);
    }
    div.appendChild(badge);
  }
  return div;
}

function getItemSeparation(msg, prevMsg) {
  if (!prevMsg || !msg) return { largeGap: true };
  const sameSender = msg.sender === prevMsg.sender;
  if (!sameSender) return { largeGap: true };
  const t1 = new Date(prevMsg.ts).valueOf();
  const t2 = new Date(msg.ts).valueOf();
  if (Math.abs(t2 - t1) >= 60000) return { largeGap: true };
  return { largeGap: false };
}

function formatTime(ts) {
  try {
    const d = new Date(ts);
    const h = d.getHours().toString().padStart(2, '0');
    const m = d.getMinutes().toString().padStart(2, '0');
    return h + ':' + m;
  } catch(e) {
    return '';
  }
}

function formatDateLabel(ts) {
  try {
    const d = new Date(ts);
    const now = new Date();
    const weekday = d.toLocaleDateString(undefined, { weekday: 'short' });
    const dayMonth = d.toLocaleDateString(undefined, {
      day: 'numeric',
      month: 'short',
      year: d.getFullYear() !== now.getFullYear() ? 'numeric' : undefined
    });
    return weekday + ', ' + dayMonth;
  } catch(e) {
    return '';
  }
}

function formatDuration(secs) {
  const m = Math.floor(secs / 60);
  const s = secs % 60;
  return m.toString().padStart(2, '0') + ':' + s.toString().padStart(2, '0');
}

function formatFileSize(bytes) {
  if (bytes < 1024) return bytes + ' B';
  if (bytes < 1024 * 1024) return (bytes / 1024).toFixed(1) + ' KB';
  return (bytes / (1024 * 1024)).toFixed(1) + ' MB';
}

document.querySelectorAll('[data-simplex-channel-preview]').forEach(initChannelPreview);

})();
