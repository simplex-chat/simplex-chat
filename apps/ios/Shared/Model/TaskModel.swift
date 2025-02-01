import Foundation

struct Task: Identifiable {
    var id: UUID
    var title: String
    var description: String
    var dueDate: Date
    var isCompleted: Bool
    var userId: UUID

    init(id: UUID = UUID(), title: String, description: String, dueDate: Date, isCompleted: Bool = false, userId: UUID) {
        self.id = id
        self.title = title
        self.description = description
        self.dueDate = dueDate
        self.isCompleted = isCompleted
        self.userId = userId
    }
}

class TaskModel: ObservableObject {
    @Published var tasks: [Task] = []

    func addTask(title: String, description: String, dueDate: Date, userId: UUID) {
        let newTask = Task(title: title, description: description, dueDate: dueDate, userId: userId)
        tasks.append(newTask)
    }

    func updateTask(task: Task) {
        if let index = tasks.firstIndex(where: { $0.id == task.id }) {
            tasks[index] = task
        }
    }

    func deleteTask(task: Task) {
        tasks.removeAll { $0.id == task.id }
    }

    func getTasks(for userId: UUID) -> [Task] {
        return tasks.filter { $0.userId == userId }
    }
}
