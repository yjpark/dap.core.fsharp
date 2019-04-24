var forge = require('node-forge');

module.exports = {
    // calcSum : string -> byte[]
    calcSum: function (content) {
        var md = forge.md.sha256.create();
        md.update(content);
        return md.digest().getBytes();
    }
};