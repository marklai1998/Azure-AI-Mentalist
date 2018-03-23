const mongoose = require('mongoose');
const collections = mongoose.model('collections');

const path = require('path');
const fs = require('fs');
const rimraf = require('rimraf');
const cmd = require('node-cmd');

exports.remove = async (id, callback) => {
    try {
        await rimraf('./public/collections/' + id, function (err) {
        });
        await collections.find({_id: id}).remove().exec();
        await callback();
    } catch (err) {
        return err
    }
};

exports.create = async (size, videos, callback) => {
    try {
        const collection = await collections({
            size: size,
            videos: videos
        }).save();
        const id = collection.id;
        await fs.mkdir('./public/collections/' + id);
        const video_name = videos.split(",");
        await video_name.map((name, count) => {
            const count_1 = count + 1;
            console.log(name);
            var source = fs.createReadStream('./public/upload/' + name + '/data.csv');
            var dest = fs.createWriteStream('./public/collections/' + id + '/data' + count_1 + '.csv');

            source.pipe(dest);
            source.on('end', function () { /* copied */
            });
            source.on('error', function (err) { /* error */
            });
        });
        await cmd.get(
            "rscript functions.R " + id,
            function (err, data, stderr) {
                console.log(err + data + stderr)
            }
        );
        await callback();
    } catch (err) {
        return err
    }
};