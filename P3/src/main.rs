use std::thread;
use std::sync::{Arc, Mutex};
use std::time::Duration;

const MAX_THREADS:usize = 4;
const BSIZE: usize = 10;
const ITERATIONS: usize = 20;

struct Buffer {
    data: [i32; BSIZE],
    head: usize,
    tail: usize,
    length: usize,
}

impl Buffer {
    pub fn new() -> Self {
        Buffer { data: [0; BSIZE], head: 0, tail: 0, length: 0 }
    }

    pub fn push(&mut self, value: i32) {
        assert!(self.length < 10);
        self.data[self.head] = value;
        self.head = (self.head + 1) % BSIZE;
        self.length += 1;
    }

    pub fn pop(&mut self) -> i32 {
        assert!(self.length > 0);
        let value = self.data[self.tail];
        self.tail = (self.tail + 1) % BSIZE;
        self.length -= 1;
        value
    }

    pub fn getLength(&self) -> usize {
        self.length
    }
}

fn produce(buffer: &Arc<Mutex<Buffer>>, id: usize) {
        
    let mut buffer = buffer.lock().unwrap();
    println!("Id: {} ; Total: {}", id, (*buffer).getLength());
    if (*buffer).getLength() < BSIZE {
        (*buffer).push(1);
    }
}

fn consume(buffer: &Arc<Mutex<Buffer>>, id: usize) {
        
    let mut buffer = buffer.lock().unwrap();
    println!("Id: {} ; Total: {}", id, (*buffer).getLength());
    if (*buffer).getLength() > 0 {
        (*buffer).pop();
    }
}

fn max(array: [i32; MAX_THREADS], id: usize) -> i32 {

    let mut result = -1;
    for i in 0..MAX_THREADS {
        if i == id { continue }
        
        if result < array[i] {result = array[i]}
    }
    result

}

fn can_enter_cs(array: [i32; MAX_THREADS], id: usize) -> bool {

    for i in 0..MAX_THREADS {
        if i == id { continue }
//        println!("thread {}, array [0] = {}, [1] = {}, [2] = {}, [3] = {}", pos, array[0], array[1], array[2], array[3]);
        if array[i] == 0 || (array[id] < array[i] || (array[id] == array[i] && id < i)) { return false }
    }
    true

}

fn producer(buffer: Arc<Mutex<Buffer>>, arr: Arc<Mutex<[i32; MAX_THREADS]>>, id: usize) {

    for _i in 0..ITERATIONS {
        let mut val = arr.lock().unwrap();
        val[id] = max(*val, id) + 1;

        while can_enter_cs(*val, id) {}
        
        produce(&buffer, id);
        val[id] = 0;

        drop(val);
        thread::sleep(Duration::from_millis(100));
    }
}

fn consumer(buffer: Arc<Mutex<Buffer>>, arr: Arc<Mutex<[i32; MAX_THREADS]>>, id: usize) {

    for _i in 0..ITERATIONS {
        let mut val = arr.lock().unwrap();
        val[id] = max(*val, id) + 1;

        while can_enter_cs(*val, id) {}
        
        consume(&buffer, id);
        val[id] = 0;

        drop(val);
        thread::sleep(Duration::from_millis(100));
    }
}

fn main() {
   
    let b = Arc::new(Mutex::new(Buffer::new()));
    let array:[i32; MAX_THREADS] = [0; MAX_THREADS];
    let ord = Arc::new(Mutex::new(array));


    let b0 = b.clone();
    let ord0 = ord.clone();
    let phandle0 = std::thread::spawn(move || producer(b0, ord0, 0));
    let b1 = b.clone();
    let ord1 = ord.clone();
    let phandle1 = std::thread::spawn(move || producer(b1, ord1, 1));
    let b2 = b.clone();
    let ord2 = ord.clone();
    let chandle0 = std::thread::spawn(move || consumer(b2, ord2, 2));
    let b3 = b.clone();
    let ord3 = ord.clone();
    let chandle1 = std::thread::spawn(move || consumer(b3, ord3, 3));

    phandle0.join().unwrap();
    phandle1.join().unwrap();
    chandle0.join().unwrap();
    chandle1.join().unwrap();

}
