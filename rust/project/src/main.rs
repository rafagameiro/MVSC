
mod waiter;

use waiter::{request_permission, free_chopstick};
use std::thread;
use std::sync::{Arc, Mutex};
use std::time::Duration;

fn eat(id: i32) {
    
    thread::sleep(Duration::from_millis(2000));

    free_chopstick(id);
}

fn ask_waiter(mutex: &Arc<Mutex<i32>>, id: i32) -> bool {
    
    let _val = mutex.lock().unwrap();
    println!("inside mutex {}", id);
    let result = request_permission(id);

    return result;
}

fn run(mutex: Arc<Mutex<i32>>, id: i32) {

    for _i in 0..10 {

        if ask_waiter(&mutex, id)  { eat(id) }

    }

}

fn main() {

    let mutex = Arc::new(Mutex::new(1));

    let mut0 = mutex.clone();
    let phandle0 = std::thread::spawn(move || run(mut0, 0));
    let mut1 = mutex.clone();
    let phandle1 = std::thread::spawn(move || run(mut1, 1));
    let mut2 = mutex.clone();
    let phandle2 = std::thread::spawn(move || run(mut2, 2));
    let mut3 = mutex.clone();
    let phandle3 = std::thread::spawn(move || run(mut3, 3));
    let mut4 = mutex.clone();
    let phandle4 = std::thread::spawn(move || run(mut4, 4));

    phandle0.join().unwrap();
    phandle1.join().unwrap();
    phandle2.join().unwrap();
    phandle3.join().unwrap();
    phandle4.join().unwrap();

}
