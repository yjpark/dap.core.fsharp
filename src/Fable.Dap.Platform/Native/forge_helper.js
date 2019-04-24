var forge = require('node-forge');

module.exports = {
    // calcSum : string -> Base64
    calcSha256Sum: function (content) {
        var md = forge.md.sha256.create();
        md.update(content);
        return forge.util.encode64(md.digest().getBytes());
    }
};
