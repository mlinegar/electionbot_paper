const wsProtocol = window.location.protocol === 'https:' ? 'wss' : 'ws';
const ws = new WebSocket(`${wsProtocol}://${window.location.host}/chat`);

// const ws = new WebSocket(`wss://${window.location.host}/chat`);
const messagesDiv = document.getElementById('messages');
const userInput = document.getElementById('user-input');
const sendButton = document.getElementById('send-button');
const stopButton = document.getElementById('stop-chat');

// let userMessageCount = 0; // Initialize message count

console.log('WebSocket connection established');

// ws.onopen = () => {
//     console.log('WebSocket connection opened');
// };
// window.ws.onopen = () => {
ws.onopen = () => {
    console.log('WebSocket connection opened');
};

let currentResponseDiv = null;
let responseMessageId = null;

// ws.onmessage = (event) => {
//     console.log('Message received from server');
//     const message = JSON.parse(event.data);
//     console.log('Message data:', message);

//     if (message.type === 'response') {
//         if (responseMessageId !== message.message_id) {
//             responseMessageId = message.message_id;
//             currentResponseDiv = document.createElement('div');
//             currentResponseDiv.className = 'message assistant';
//             messagesDiv.appendChild(currentResponseDiv);
//         }

//         currentResponseDiv.textContent += message.data;

//         if (message.complete) {
//             currentResponseDiv = null;
//             responseMessageId = null;
//             console.log('Response complete');
        
//             if (message.end_script) {
//                 console.log('Script ended. Redirecting to survey.');
//                 const redirectUrl = message.redirect_url || 'chat_assets/survey-complete.html';
//                 startCountdown(10);
//                 setTimeout(() => {
//                     window.location.href = redirectUrl;
//                 }, 10000);  // Redirect after 10 seconds
//             } else {
//                 // Focus on the input field after a short delay
//                 setTimeout(() => {
//                     userInput.focus();
//                 }, 100);
//             }
//         }
//         console.log('Appended response to messages');
//         messagesDiv.scrollTop = messagesDiv.scrollHeight; // Auto-scroll to the bottom
//     }
// };

// ws.onmessage = (event) => {
//     console.log('Message received from server');
//     const message = JSON.parse(event.data);
//     console.log('Message data:', message);

//     if (message.type === 'response') {
//         if (responseMessageId !== message.message_id) {
//             responseMessageId = message.message_id;
//             currentResponseDiv = document.createElement('div');
//             currentResponseDiv.className = 'message assistant';
//             messagesDiv.appendChild(currentResponseDiv);
//         }

//         currentResponseDiv.textContent += message.data;

//         if (message.complete) {
//             currentResponseDiv = null;
//             responseMessageId = null;
//             console.log('Response complete');
        
//             if (message.end_script) {
//                 console.log('Script ended. Starting countdown.');
//                 const redirectUrl = message.redirect_url || 'chat_assets/survey-complete.html';
//                 console.log('Redirect URL:', redirectUrl);
//                 startCountdown(10, redirectUrl);
//             } else {
//                 // Focus on the input field after a short delay
//                 setTimeout(() => {
//                     userInput.focus();
//                 }, 100);
//             }
//         }
//         console.log('Appended response to messages');
//         messagesDiv.scrollTop = messagesDiv.scrollHeight; // Auto-scroll to the bottom
//     }
// };

// function startCountdown(seconds, redirectUrl) {
//     console.log('Countdown started');
//     const countdownElement = document.getElementById('countdown');
//     if (!countdownElement) {
//         console.error('Countdown element not found');
//         return;
//     }
//     countdownElement.style.display = 'block';
//     countdownElement.textContent = `Redirecting in ${seconds} seconds...`;
    
//     function updateCountdown() {
//         console.log(`Countdown: ${seconds} seconds remaining`);
//         seconds--;
//         if (seconds > 0) {
//             countdownElement.textContent = `Redirecting in ${seconds} seconds...`;
//             setTimeout(updateCountdown, 1000);
//         } else {
//             console.log('Countdown finished, redirecting to:', redirectUrl);
//             window.location.href = redirectUrl;
//         }
//     }
    
//     setTimeout(updateCountdown, 1000);
// }

// ws.onmessage = (event) => {
//     console.log('Message received from server');
//     const message = JSON.parse(event.data);
//     console.log('Full message data:', message);

//     if (message.type === 'response') {
//         if (responseMessageId !== message.message_id) {
//             responseMessageId = message.message_id;
//             currentResponseDiv = document.createElement('div');
//             currentResponseDiv.className = 'message assistant';
//             messagesDiv.appendChild(currentResponseDiv);
//         }

//         currentResponseDiv.textContent += message.data;

//         if (message.complete) {
//             currentResponseDiv = null;
//             responseMessageId = null;
//             console.log('Response complete. End script:', message.end_script);
        
//             if (message.end_script) {
//                 console.log('Script ended. Starting countdown.');
//                 const redirectUrl = message.redirect_url || 'chat_assets/survey-complete.html';
//                 console.log('Redirect URL:', redirectUrl);
//                 startCountdown(10, redirectUrl);
//             } else {
//                 // Focus on the input field after a short delay
//                 setTimeout(() => {
//                     userInput.focus();
//                 }, 100);
//             }
//         }
//         console.log('Appended response to messages');
//         messagesDiv.scrollTop = messagesDiv.scrollHeight; // Auto-scroll to the bottom
//     }
// };

// function startCountdown(seconds, redirectUrl) {
//     console.log('Countdown started');
//     const countdownElement = document.getElementById('countdown');
//     if (!countdownElement) {
//         console.error('Countdown element not found');
//         return;
//     }
//     countdownElement.style.display = 'block';
//     countdownElement.textContent = `Redirecting in ${seconds} seconds...`;
    
//     function updateCountdown() {
//         console.log(`Countdown: ${seconds} seconds remaining`);
//         seconds--;
//         if (seconds > 0) {
//             countdownElement.textContent = `Redirecting in ${seconds} seconds...`;
//             setTimeout(updateCountdown, 1000);
//         } else {
//             console.log('Countdown finished, redirecting to:', redirectUrl);
//             window.location.href = redirectUrl;
//         }
//     }
    
//     setTimeout(updateCountdown, 1000);
// }

ws.onmessage = (event) => {
    console.log('Message received from server');
    const message = JSON.parse(event.data);
    console.log('Full message data:', message);

    if (message.type === 'response') {
        if (responseMessageId !== message.message_id) {
            responseMessageId = message.message_id;
            currentResponseDiv = document.createElement('div');
            currentResponseDiv.className = 'message assistant';
            messagesDiv.appendChild(currentResponseDiv);
        }

        currentResponseDiv.textContent += message.data;

        if (message.complete) {
            currentResponseDiv = null;
            responseMessageId = null;
            console.log('Response complete. End script:', message.end_script);
        
            if (message.end_script) {
                console.log('Script ended. Starting countdown.');

                const redirectUrl = message.redirect_url || 'chat_assets/survey-complete.html';
                console.log('Redirect URL:', redirectUrl);

                // Start countdown before redirecting
                startCountdown(10, redirectUrl);  // Call countdown function
            } else {
                // Focus on the input field after a short delay
                setTimeout(() => {
                    userInput.focus();
                }, 100);
            }
        }
        console.log('Appended response to messages');
        messagesDiv.scrollTop = messagesDiv.scrollHeight; // Auto-scroll to the bottom
    }
};

function startCountdown(seconds, redirectUrl) {
    console.log('Countdown started');
    const countdownElement = document.getElementById('countdown');
    if (!countdownElement) {
        console.error('Countdown element not found');
        return;
    }
    countdownElement.style.display = 'block';
    countdownElement.textContent = `Redirecting in ${seconds} seconds...`;
    
    function updateCountdown() {
        console.log(`Countdown: ${seconds} seconds remaining`);
        seconds--;
        if (seconds > 0) {
            countdownElement.textContent = `Redirecting in ${seconds} seconds...`;
            setTimeout(updateCountdown, 1000);
        } else {
            console.log('Countdown finished, redirecting to:', redirectUrl);
            window.location.href = redirectUrl;
        }
    }
    
    setTimeout(updateCountdown, 1000);
}

ws.onerror = (error) => {
    console.error('WebSocket error observed:', error);
};

ws.onclose = (event) => {
    console.log('WebSocket connection closed:', event);
};


function sendMessage(text) {
    const userDiv = document.createElement('div');
    userDiv.className = 'message user';
    const contentDiv = document.createElement('div');
    contentDiv.className = 'message-content';
    contentDiv.textContent = text;
    userDiv.appendChild(contentDiv);
    messagesDiv.appendChild(userDiv);
    console.log(`Sent message: ${text}`);

    ws.send(JSON.stringify({ type: 'input', text: text }));
    userInput.value = '';
    
    // Call the function defined in index.html to increment the message count
    incrementMessageCount();
    // Scroll to the bottom of the messages div
    messagesDiv.scrollTop = messagesDiv.scrollHeight;
}

userInput.onkeydown = (event) => {
    if (event.key === 'Enter' && !event.shiftKey) {
        event.preventDefault();
        sendButton.click();
    }
};


// Function to handle window resize and orientation change
function handleResize() {
    // Adjust the height of the messages div to account for the keyboard
    const windowHeight = window.innerHeight;
    const toolbarHeight = document.querySelector('.toolbar').offsetHeight;
    const inputHeight = document.querySelector('.input').offsetHeight;
    messagesDiv.style.height = `${windowHeight - toolbarHeight - inputHeight}px`;
}

// Add event listeners for resize and orientation change
window.addEventListener('resize', handleResize);
window.addEventListener('orientationchange', handleResize);

// Initial call to set the correct height
handleResize();

// Function to attempt refocusing the input field
function attemptRefocus() {
    if (document.activeElement !== userInput) {
        userInput.focus();
    }
}

// Add touch event listeners to the document
document.addEventListener('touchstart', attemptRefocus);
document.addEventListener('touchend', attemptRefocus);
