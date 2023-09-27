self.addEventListener('install', function(event) {
  self.skipWaiting();
  console.log("EventListener installed.");
});

self.addEventListener('activate', function(event) {
    console.log("EventListener activated.");
});

self.addEventListener('push', function(event) {
    console.log("Received a push message: ", event);
    if (event.data) {
        var pushData = event.data.json();
        event.waitUntil(
            self.registration.showNotification(pushData.title, {
                body: pushData.body,
                icon: pushData.icon,
                tag: pushData.tag,
                data: pushData,
                renotify: true
            }));
    }
});

// TODO need a message for updating the service worker
// TODO need a message for updating the subscription

self.addEventListener('notificationclick', function(event) {
    console.log("Clicked on notification: ", event);
    event.notification.close();
    var url = event.notification.data.url;
    if (clients.openWindow) {
        return clients.openWindow(url);
    }
});
