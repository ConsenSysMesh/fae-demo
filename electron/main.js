const electron = require('electron')
// Used to spawn processes
const child_process = require('child_process')
// Module to control application life.
const app = electron.app
// Module to create native browser window.
const BrowserWindow = electron.BrowserWindow

// Keep a global reference of the window object, if you don't, the window will
// be closed automatically when the JavaScript object is garbage collected.
let mainWindow
// Do the same for the backend web server
let backendServer
let faeserver

function createWindow() {
  // Create the browser window.
  mainWindow = new BrowserWindow({ width: 1024, height: 800 })

  // and load the index.html of the app.
  mainWindow.loadURL('file://' + __dirname + '/resources/client/index.html')

  // Emitted when the window is closed.
  mainWindow.on('closed', function () {
    // Dereference the window object, usually you would store windows
    // in an array if your app supports multi windows, this is the time
    // when you should delete the corresponding element.
    mainWindow = null
  })
}

function createBackendServer() {
  backendServer = child_process.spawn('./resources/server/auction-server-exe')
  backendServer.stdout.on('data', function (data) {
    console.log(data.toString());
  });
  backendServer.stderr.on('data', function (data) {
    console.log(data.toString());
  });

  return backendServer
}

function createFaeserver() {
  faeServer = child_process.spawn('./resources/faeserver/faeServer.sh')
  faeServer.stdout.on('data', data => {
    console.log(data.toString());
  });
  faeServer.stderr.on('data', data => {
    console.log(data.toString());
  });

  return faeServer
}

// This method will be called when Electron has finished
// initialization and is ready to create browser windows.
// Some APIs can only be used after this event occurs.
app.on('ready', createWindow)

// Start the backend web server and faeserver when Electron has finished initializing
app.on('ready', () => {
  createBackendServer()
  createFaeserver()
})

// Close the server when the application is shut down
app.on('will-quit', () => {
  if (backendServer) {
    backendServer.kill()
  }
  if (faeserver) {
    faeserver.kill()
  }
})

// Quit when all windows are closed.
app.on('window-all-closed', () => {
  // On OS X it is common for applications and their menu bar
  // to stay active until the user quits explicitly with Cmd + Q
  if (process.platform !== 'darwin') {
    app.quit()
  }
})

app.on('activate', () => {
  // On OS X it's common to re-create a window in the app when the
  // dock icon is clicked and there are no other windows open.
  if (mainWindow === null) {
    createWindow()
  }
})