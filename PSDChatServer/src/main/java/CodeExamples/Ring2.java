import co.paralleluniverse.fibers.SuspendExecution;
import co.paralleluniverse.actors.*;
import java.util.concurrent.TimeUnit;

//Esta classe faz o mesmo que Ring, mas está organizada de forma mais simpática
public class Ring2 {
    static final int N = 1000;
    static final int M = 1000;

    public static void main(String args[]) throws Exception {
        for (int i = 0; i < 5; i++)
            run();
    }

    static void run() throws Exception {
        final long start = System.nanoTime();
        Manager manager = new Manager();
        manager.spawn();
        int totalCount = manager.get();
        final long time = TimeUnit.MILLISECONDS.convert(System.nanoTime() - start, TimeUnit.NANOSECONDS);
        System.out.println("messages: " + totalCount + " time (ms): " + time);
    }

    static class Manager extends BasicActor<Integer, Integer> {
        @Override
        protected Integer doRun() throws InterruptedException, SuspendExecution {
            ActorRef<Integer> a = this.ref();
            for (int i = 0; i < N - 1; i++)
                a = new Relay(a).spawn();
            a.send(1);
            Integer msg = null;
            for (int i = 0; i < M; i++) {
                msg = receive();
                a.send(msg + 1);
            }
            return msg;
        }
    }

    static class Relay extends BasicActor<Integer, Void> {
        final ActorRef<Integer> prev;

        public Relay(ActorRef<Integer> prev) { this.prev = prev; }
 
        @Override
        protected Void doRun() throws InterruptedException, SuspendExecution {
            for (;;) {
                Integer m = receive();
                prev.send(m + 1);
            }
        }
    }

}

