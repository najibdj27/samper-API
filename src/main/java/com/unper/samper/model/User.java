package com.unper.samper.model;

import java.util.Date;
import java.util.Set;
import java.util.HashSet;

import javax.persistence.*;

import com.unper.samper.model.common.Audit;

import lombok.*;

@AllArgsConstructor
@NoArgsConstructor
@Getter
@Setter
@Builder
@Entity
@Table(name = "user", schema = "public", uniqueConstraints = {@UniqueConstraint(columnNames = "username"), @UniqueConstraint(columnNames = "email"), @UniqueConstraint(columnNames = "phoneNumber")})
public class User extends Audit {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    private String firstName;

    private String lastName;

    private Date dateOfBirth;
    
    private String username;

    private String email;

    private String phoneNumber;

    private String password;

    @Builder.Default
    @ManyToMany(fetch = FetchType.LAZY)
    @JoinTable(name = "user_role", joinColumns = @JoinColumn(name = "user_id"), inverseJoinColumns = @JoinColumn(name = "role_id"))
    private Set<Role> roles = new HashSet<>();
    
    @Builder.Default
    private Boolean isDeleted = Boolean.FALSE;
}
