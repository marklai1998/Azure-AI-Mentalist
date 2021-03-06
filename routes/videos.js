const express = require('express');
const router = express.Router();

const db = require('../db/videos');
const db2 = require('../db/collections');
const mongoose = require('mongoose');
const videos = mongoose.model('videos');
const collections = mongoose.model('collections');

const local_file = require('../function/local_file');
const create_collection = require('../function/create_collection');

router.get('/', function (req, res, next) {
    if (req.session.username) {
        let show_message = false;
        let message = "";
        switch (req.session.message) {
            case "no_file":
                show_message = true;
                message = "No file selected";
                break;
            case "no_select":
                show_message = true;
                message = "No video selected";
                break;
            case "success":
                show_message = true;
                message = "Upload success";
                break;
            case "collection_success":
                show_message = true;
                message = "Collection created";
                break;
            case "error":
                show_message = true;
                message = "Error";
                break;
            case "delete":
                show_message = true;
                message = "Video deleted";
                break;
        }
        const find_all_videos = async () => {
            try {
                const all_videos = await videos.find();
                res.render('videos', {
                    title: 'Videos',
                    show_message: show_message,
                    message: message,
                    username: req.session.username,
                    videos: all_videos
                });
            } catch (err) {
                return err;
            }
        };
        console.log(find_all_videos());
        req.session.message = null;
    } else res.redirect('/login')
});

router.get('/message/:message', function (req, res, next) {
    if (req.session.username) {
        req.session.message = req.params.message;
        res.redirect("/videos");
    } else res.redirect('/login');
});

router.get('/delete/:_id', function (req, res, next) {
    if (req.session.username) {
        console.log(local_file.remove(req.params._id, function () {
            res.redirect("/videos/message/delete");
        }));
    } else res.redirect('/login');
});

router.post('/upload', function (req, res, next) {
    if (req.session.username) {
        if (!req.files) {
            res.redirect("/videos/message/no_file");
        }
        if (req.files.video.length > 1) {
            req.files.video.forEach(function (upload_video) {
                console.log(local_file.upload(upload_video,req.headers['content-length'], res));
            });
        } else {
            console.log(local_file.upload(req.files.video,req.headers['content-length'], res));
        }
    } else res.redirect('/login')
});

router.post('/collection', function (req, res, next) {
    if (req.session.username) {
        if (!req.body.collection || req.body.collection.length === 0) {
            res.redirect("/videos/message/no_select");
        } else {
            console.log(create_collection.create(req.body.collection.length, req.body.collection.toString(), function () {
                res.redirect("/videos/message/collection_success");
            }));
        }
    } else res.redirect('/login')
});

module.exports = router;
