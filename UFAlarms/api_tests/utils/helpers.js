
module.exports = {
    getCsrfToken: function (res) {
        const csrfToken = res.headers["netsense-csrf-token"] ? res.headers["netsense-csrf-token"] : "";
        return csrfToken;
        /* // To be used if CSRF token is sent in cookie
        const csrfTokenCookieName = 'netsense-csrf-token';
        const setCookies = res.headers["set-cookie"];
        if (setCookies) {
            for (setCookie of setCookies) {
                if (setCookie.indexOf(csrfTokenCookieName) !== -1) {
                    const csrfCookies = setCookie.split(';')[0];
                    const csrfTokens = csrfCookies.split('=');
                    if (csrfTokens[0] === csrfTokenCookieName) {
                        //console.log('csrfToken', csrfTokens[1]);
                        return csrfTokens[1];
                    }
                }
            }
        }
        return ""; */
    }
}