use lazy_static::lazy_static;
use std::sync::Mutex;

const MAX_THREADS:usize = 5;

lazy_static! {

    static ref ARRAY: Mutex<[i32; MAX_THREADS]> = Mutex::new([0; MAX_THREADS]);
    static ref CHOPSTICK: Mutex<[bool; MAX_THREADS]> = Mutex::new([false; MAX_THREADS]);

}


pub fn request_permission(id: i32) -> bool {

    let mut c = CHOPSTICK.lock().unwrap();
    let mut result:bool = false;
    println!("requesting chopsticks {}", id);
    match id {

        0 => {

            if !(c[0] || c[4]) {
                c[0] = true;
                c[4] = true;
                
                result = true;
            }
        }

        1 => {

            if !(c[1] || c[0]) {
                c[1] = true;
                c[0] = true;

                result = true;
            }
        }

        2 => {

            if !(c[2] || c[1]) {
                c[2] = true;
                c[1] = true;

                result = true;
            }
        }

        3 => {

            if !(c[3] || c[2]) {
                c[3] = true;
                c[2] = true;

                result = true;
            }
        }

        4 => {

            if !(c[4] || c[3]) {
                c[4] = true;
                c[3] = true;

                result = true;
            }
        }

        _ => result = false
    }

    if result {println!("access given {}", id)}

    return result;

}

pub fn free_chopstick(id: i32) {

    let mut c = CHOPSTICK.lock().unwrap();
    match id {

        0 => {
            c[0] = false;
            c[4] = false;
        }

        1 => {
            c[1] = false;
            c[0] = false;
        }

        2 => {
            c[2] = false;
            c[1] = false;
        }

        3 => {
            c[3] = false;
            c[2] = false;
        }

        4 => {
            c[4] = false;
            c[3] = false;
        }

        _ =>  ()
    }
}


